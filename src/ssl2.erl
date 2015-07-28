%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Main API module for SSL2.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl2).
-author("Konrad Zemek").

-record(sock_ref, {
    socket :: ssl2_nif:socket(),
    supervisor :: pid(),
    receiver :: pid(),
    sender :: pid()
}).

-record(acceptor_ref, {
    acceptor :: ssl2_nif:acceptor()
}).

%% API
-export([connect/3, connect/4, send/2, recv/2, recv/3, listen/2,
    accept/1, accept/2, handshake/1, handshake/2, setopts/2,
    controlling_process/2, peername/1, sockname/1, close/1, peercert/1,
    certificate_chain/1, shutdown/2]).

%% Types
-type der_encoded() :: binary().
-type pem_encoded() :: binary().
-type str() :: binary() | string().

-type option() ::
{packet, raw | 0 | 1 | 2 | 4} |
{active, boolean() | once} |
{exit_on_close, boolean()}.

-type ssl_option() ::
{verify_type, verify_none | verify_peer} |
{fail_if_no_peer_cert, boolean()} |
{verify_client_once, boolean()} |
{rfc2818_verification_hostname, str()} |
{cacerts, [pem_encoded()]} |
{crls, [pem_encoded()]} |
{certfile, str()} |
{keyfile, str()} |
{chain, [pem_encoded()]}.

-opaque socket() :: #sock_ref{}.
-opaque acceptor() :: #acceptor_ref{}.

-export_type([option/0, ssl_option/0, socket/0, acceptor/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv connect(Host, Port, Opts, infinity)
%% @end
%%--------------------------------------------------------------------
-spec connect(Host :: str(), Port :: inet:port_number(),
    Opts :: [option() | ssl_option()]) ->
    {ok, Socket :: socket()} |
    {error, Reason :: atom()}.
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Opens an ssl connection to Host, Port.
%% @end
%%--------------------------------------------------------------------
-spec connect(Host :: str(), Port :: inet:port_number(),
    Opts :: [option() | ssl_option()], Timeout :: timeout()) ->
    {ok, Socket :: socket()} |
    {error, Reason :: atom()}.
connect(Host, Port, Options, Timeout) ->
    Ref = make_ref(),

    {CertPath, KeyPath, VerifyType, FailIfNoPeerCert, VerifyClientOnce,
        RFC2818Hostname, CAs, CRLs, Chain} = extract_tls_settings(Options),

    case ssl2_nif:connect(Ref, Host, Port, CertPath, KeyPath, VerifyType,
        FailIfNoPeerCert, VerifyClientOnce, RFC2818Hostname,
        CAs, CRLs, Chain) of
        ok ->
            receive
                {Ref, {ok, Sock}} -> start_socket_processes(Sock, Options);
                {Ref, Result} -> Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} when is_atom(Reason) ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Writes Data to Socket.
%% If the socket is closed, returns {error, closed}.
%% @end
%%--------------------------------------------------------------------
-spec send(Socket :: socket(), Data :: iodata()) ->
    ok | {error, Reason :: closed | atom()}.
send(#sock_ref{sender = Sender}, Data) ->
    try
        gen_fsm:sync_send_event(Sender, {send, Data}, infinity)
    catch
        exit:{Reason, _} when Reason =:= noproc; Reason =:= shutdown ->
            {error, closed}
    end.

%%--------------------------------------------------------------------
%% @equiv recv(Socket, Size, infinity)
%% @end
%%--------------------------------------------------------------------
-spec recv(Socket :: socket(), Size :: non_neg_integer()) ->
    {ok, binary()} |
    {error, Reason :: closed | timeout | atom()}.
recv(SockRef, Size) ->
    recv(SockRef, Size, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Receives a packet from a socket in passive mode.
%% If the socket is closed, returns {error, closed}.
%% @end
%%--------------------------------------------------------------------
-spec recv(Socket :: socket(), Size :: non_neg_integer(),
    Timeout :: timeout()) ->
    {ok, binary()} |
    {error, Reason :: closed | timeout | atom()}.
recv(#sock_ref{receiver = Receiver}, Size, Timeout) ->
    try
        gen_fsm:sync_send_event(Receiver, {recv, Size, Timeout}, infinity)
    catch
        exit:{Reason, _} when Reason =:= noproc; Reason =:= shutdown ->
            {error, closed}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates an acceptor (listen socket).
%% @end
%%--------------------------------------------------------------------
-spec listen(Port :: inet:port_number(), Opts :: [ssl_option()]) ->
    {ok, Acceptor :: acceptor()} |
    {error, Reason :: atom()}.
listen(Port, Options) ->
    true = proplists:is_defined(certfile, Options),

    {CertPath, KeyPath, VerifyType, FailIfNoPeerCert, VerifyClientOnce,
        RFC2818Hostname, CAs, CRLs, Chain} = extract_tls_settings(Options),

    case ssl2_nif:listen(Port, CertPath, KeyPath, VerifyType, FailIfNoPeerCert,
        VerifyClientOnce, RFC2818Hostname, CAs, CRLs, Chain) of
        {ok, Acceptor} -> {ok, #acceptor_ref{acceptor = Acceptor}};
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @equiv accept(Acceptor, infinity)
%% @end
%%--------------------------------------------------------------------
-spec accept(Acceptor :: acceptor()) ->
    {ok, Socket :: socket()} |
    {error, Reason :: timeout | atom()}.
accept(AcceptorRef) ->
    accept(AcceptorRef, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Accepts an incoming connection on an acceptor.
%% The returned socket should be passed to ssl2:handshake to establish
%% the secure connection.
%% @end
%%--------------------------------------------------------------------
-spec accept(Acceptor :: acceptor(), Timeout :: timeout()) ->
    {ok, Socket :: socket()} |
    {error, Reason :: timeout | atom()}.
accept(#acceptor_ref{acceptor = Acceptor}, Timeout) ->
    Ref = make_ref(),
    case ssl2_nif:accept(Ref, Acceptor) of
        ok ->
            receive
                {Ref, {ok, Sock}} -> start_socket_processes(Sock, []);
                {Ref, Result} -> Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} when is_atom(Reason) ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @equiv handshake(Socket, infinity)
%% @end
%%--------------------------------------------------------------------
-spec handshake(Socket :: socket()) -> ok | {error, Reason :: atom()}.
handshake(Socket) ->
    handshake(Socket, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Performs a TLS handshake on the new TCP socket.
%% The socket should be created by ssl2:accept .
%% @end
%%--------------------------------------------------------------------
-spec handshake(Socket :: socket(), Timeout :: timeout()) ->
    ok | {error, Reason :: timeout | any()}.
handshake(#sock_ref{socket = Sock}, Timeout) ->
    Ref = make_ref(),
    case ssl2_nif:handshake(Ref, Sock) of
        ok ->
            receive
                {Ref, Result} -> Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} when is_atom(Reason) ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets options according to Options for the socket Socket.
%% @end
%%--------------------------------------------------------------------
-spec setopts(Socket :: socket(), Opts :: [option()]) -> ok.
setopts(#sock_ref{receiver = Receiver, sender = Sender}, Options) ->
    gen_fsm:send_all_state_event(Receiver, {setopts, Options}),
    gen_fsm:send_all_state_event(Sender, {setopts, Options}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Assigns a new controlling process to the socket.
%% A controlling process receives all messages from the socket.
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(Socket :: socket(), NewControllingProcess :: pid()) ->
    ok.
controlling_process(#sock_ref{receiver = Receiver}, Pid) ->
    gen_fsm:send_all_state_event(Receiver, {controlling_process, Pid}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns the address and port number of the peer.
%% @end
%%--------------------------------------------------------------------
-spec peername(Socket :: socket()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, Reason :: atom()}.
peername(#sock_ref{socket = Sock}) ->
    Ref = make_ref(),
    case ssl2_nif:peername(Ref, Sock) of
        ok -> receive {Ref, Result} -> parse_name_result(Result) end;
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns the address and port number of the socket.
%% @end
%%--------------------------------------------------------------------
-spec sockname(SocketOrAcceptor :: socket() | acceptor()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, Reason :: atom()}.
sockname(#sock_ref{socket = Sock}) ->
    Ref = make_ref(),
    case ssl2_nif:sockname(Ref, Sock) of
        ok -> receive {Ref, Result} -> parse_name_result(Result) end;
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end;
sockname(#acceptor_ref{acceptor = Acceptor}) ->
    Ref = make_ref(),
    case ssl2_nif:acceptor_sockname(Ref, Acceptor) of
        ok -> receive {Ref, Result} -> parse_name_result(Result) end;
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gracefully closes the socket.
%% @end
%%--------------------------------------------------------------------
-spec close(Socket :: socket()) -> ok | {error, Reason :: atom()}.
close(#sock_ref{socket = Sock, supervisor = Sup}) ->
    ok = supervisor:terminate_child(ssl2_sup, Sup),
    Ref = make_ref(),
    case ssl2_nif:close(Ref, Sock) of
        ok -> receive {Ref, Result} -> Result end;
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a DER-encoded public certificate of the peer.
%% @end
%%--------------------------------------------------------------------
-spec peercert(Socket :: socket()) ->
    {ok, der_encoded()} |
    {error, Reason :: no_peer_certificate | atom()}.
peercert(SockRef) ->
    case certificate_chain(SockRef) of
        {ok, []} -> {error, no_peer_certificate};
        {ok, Chain} -> {ok, lists:last(Chain)};
        Result -> Result
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a DER-encoded chain of peer certificates.
%% @end
%%--------------------------------------------------------------------
-spec certificate_chain(Socket :: socket()) ->
    {ok, [der_encoded()]} | {error, Reason :: atom()}.
certificate_chain(#sock_ref{socket = Sock}) ->
    case ssl2_nif:certificate_chain(Sock) of
        {ok, Chain} -> {ok, Chain};
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Shuts down the connection in one or two directions.
%% To be able to handle that the peer has done a shutdown on the write
%% side, the {exit_on_close, false} option is useful.
%% @end
%%--------------------------------------------------------------------
-spec shutdown(Socket :: socket(), Type :: read | write | read_write) ->
    ok | {error, Reason :: atom()}.
shutdown(SockRef, Type) ->
    #sock_ref{supervisor = Sup, socket = Sock} = SockRef,

    case Type of
        read -> ok = supervisor:terminate_child(Sup, receiver);
        write -> ok = supervisor:terminate_child(Sup, sender);
        read_write ->
            ok = supervisor:terminate_child(Sup, receiver),
            ok = supervisor:terminate_child(Sup, sender)
    end,

    Ref = make_ref(),
    case ssl2_nif:shutdown(Ref, Sock, Type) of
        ok -> receive {Ref, Result} -> Result end;
        {error, Reason} when is_atom(Reason) -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts process supervisor for the NIF socket.
%% Returns a reference for the socket that is usable by the client.
%% @end
%%--------------------------------------------------------------------
-spec start_socket_processes(Socket :: ssl2_nif:socket(),
    Options :: [option()]) ->
    {ok, SockRef :: socket()}.
start_socket_processes(Sock, Options) ->
    Args = [Sock, Options, self()],
    {ok, Sup} = supervisor:start_child(ssl2_sup, Args),

    Children = supervisor:which_children(Sup),
    {_, Receiver, _, _} = lists:keyfind(receiver, 1, Children),
    {_, Sender, _, _} = lists:keyfind(sender, 1, Children),

    SockRef = #sock_ref{socket = Sock, supervisor = Sup,
        receiver = Receiver, sender = Sender},

    gen_fsm:send_all_state_event(Receiver, {sock_ref, SockRef}),

    {ok, SockRef}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses the results of ssl2_nif:peername and ssl2_nif:sockname functions.
%% @end
%%--------------------------------------------------------------------
-spec parse_name_result({ok, {StrAddress :: string(),
    Port :: inet:port_number()}}) ->
    {ok, {inet:ip_address(), inet:port_number()}};
    ({error, Reason}) -> {error, Reason} when Reason :: any().
parse_name_result({ok, {StrAddress, Port}}) ->
    {ok, Addr} = inet:parse_ipv4_address(StrAddress),
    {ok, {Addr, Port}};
parse_name_result(Result) ->
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts common TLS settings out of a proplist.
%% @end
%%--------------------------------------------------------------------
-spec extract_tls_settings(Opts :: proplists:proplist()) ->
    {str(), str(), str(), boolean(), boolean(), str(),
        [pem_encoded()], [pem_encoded()], [pem_encoded()]}.
extract_tls_settings(Opts) ->
    CertPath = proplists:get_value(certfile, Opts, ""),
    KeyPath = proplists:get_value(keyfile, Opts, CertPath),

    VerifyTypeAtom = proplists:get_value(verify_type, Opts, verify_none),
    VerifyType = atom_to_list(VerifyTypeAtom),

    FailIfNoPeerCert = proplists:get_bool(fail_if_no_peer_cert, Opts),
    VerifyClientOnce = proplists:get_bool(verify_client_once, Opts),
    RFC2818Hostname =
        proplists:get_value(rfc2818_verification_hostname, Opts, ""),

    CAs = proplists:get_value(cacerts, Opts, []),
    CRLs = proplists:get_value(crls, Opts, []),
    Chain = proplists:get_value(chain, Opts, []),

    {CertPath, KeyPath, VerifyType, FailIfNoPeerCert, VerifyClientOnce,
        RFC2818Hostname, CAs, CRLs, Chain}.
