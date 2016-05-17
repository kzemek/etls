%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% An implementation of ranch_transport behavior on top of ssl2.
%%% For function documentation check Ranch's ranch_transport docs.
%%% @end
%%%-------------------------------------------------------------------
-module(ranch_ssl2).
-author("Konrad Zemek").

%% API
-export([name/0, secure/0, messages/0, listen/1, accept/2, accept_ack/2,
    connect/3, connect/4, recv/3, send/2, setopts/2, controlling_process/2,
    peername/1, sockname/1, shutdown/2, close/1, sendfile/2, sendfile/4,
    sendfile/5]).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback name/0.
%% @end
%%--------------------------------------------------------------------
-spec name() -> atom().
name() ->
    ssl2.

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback secure/0.
%% @end
%%--------------------------------------------------------------------
-spec secure() -> boolean().
secure() ->
    true.

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback messages/0.
%% @end
%%--------------------------------------------------------------------
-spec messages() -> {OK :: atom(), Closed :: atom(), Error :: atom()}.
messages() ->
    {ssl2, ssl2_closed, ssl2_error}.

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback listen/1.
%% @end
%%--------------------------------------------------------------------
-spec listen([ssl2:option() | ssl2:ssl_option()]) ->
    {ok, ssl2:socket()} |{error, atom()}.
listen(Opts) ->
    Port = proplists:get_value(port, Opts),
    ssl2:listen(Port, Opts).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback accept/2.
%% @end
%%--------------------------------------------------------------------
-spec accept(ssl2:acceptor(), timeout()) ->
    {ok, ssl2:socket()} |
    {error, closed | timeout | atom()}.
accept(Socket, Timeout) ->
    ssl2:accept(Socket, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback accept_ack/2.
%% @end
%%--------------------------------------------------------------------
-spec accept_ack(ssl2:socket(), timeout()) -> ok.
accept_ack(Socket, Timeout) ->
    case ssl2:handshake(Socket, Timeout) of
        ok ->
            ok;
    %% Socket most likely stopped responding, don't error out.
        {error, Reason} when Reason =:= timeout; Reason =:= closed ->
            ok = close(Socket),
            exit(normal);
        {error, Reason} ->
            ok = close(Socket),
            error(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback connect/3.
%% @end
%%--------------------------------------------------------------------
-spec connect(string(), inet:port_number(),
    [ssl2:option() | ssl2:ssl_option()]) ->
    {ok, ssl2:socket()} |
    {error, atom()}.
connect(Host, Port, Opts) ->
    connect(Host, Port, Opts, infinity).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback connect/4.
%% @end
%%--------------------------------------------------------------------
-spec connect(string(), inet:port_number(), [ssl2:option() | ssl2:ssl_option()],
    timeout()) ->
    {ok, ssl2:socket()} |
    {error, atom()}.
connect(Host, Port, Opts, Timeout) ->
    ssl2:connect(Host, Port, Opts, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback recv/3.
%% @end
%%--------------------------------------------------------------------
-spec recv(ssl2:socket(), non_neg_integer(), timeout()) ->
    {ok, any()} |
    {error, closed | timeout | atom()}.
recv(Socket, Size, Timeout) ->
    ssl2:recv(Socket, Size, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback send/2.
%% @end
%%--------------------------------------------------------------------
-spec send(ssl2:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Data) ->
    ssl2:send(Socket, Data).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback setopts/2.
%% @end
%%--------------------------------------------------------------------
-spec setopts(ssl2:socket(), [ssl2:option() | ssl2:ssl_option()]) ->
    ok | {error, atom()}.
setopts(Socket, Opts) ->
    ssl2:setopts(Socket, Opts).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback controlling_process/2.
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(ssl2:socket(), pid()) ->
    ok | {error, closed | atom()}.
controlling_process(Socket, Pid) ->
    ssl2:controlling_process(Socket, Pid).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback peername/1.
%% @end
%%--------------------------------------------------------------------
-spec peername(ssl2:socket()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, atom()}.
peername(Socket) ->
    ssl2:peername(Socket).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback sockname/1.
%% @end
%%--------------------------------------------------------------------
-spec sockname(ssl2:socket() | ssl2:acceptor()) ->
    {ok, {inet:ip_address(), inet:port_number()}} |
    {error, atom()}.
sockname(Socket) ->
    ssl2:sockname(Socket).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback shutdown/2.
%% @end
%%--------------------------------------------------------------------
-spec shutdown(ssl2:socket(), read | write | read_write) ->
    ok | {error, atom()}.
shutdown(Socket, Type) ->
    ssl2:shutdown(Socket, Type).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback close/1.
%% @end
%%--------------------------------------------------------------------
-spec close(ssl2:socket()) -> ok.
close(Socket) ->
    ssl2:close(Socket).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback sendfile/2.
%% @end
%%--------------------------------------------------------------------
-spec sendfile(ssl2:socket(), file:name() | file:fd()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, Filename) ->
    sendfile(Socket, Filename, 0, 0, []).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback sendfile/4.
%% @end
%%--------------------------------------------------------------------
-spec sendfile(ssl2:socket(), file:name() | file:fd(),
    non_neg_integer(), non_neg_integer()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
    sendfile(Socket, File, Offset, Bytes, []).

%%--------------------------------------------------------------------
%% @doc
%% {@link ranch_transport} callback sendfile/5.
%% @end
%%--------------------------------------------------------------------
-spec sendfile(ssl2:socket(), file:name() | file:fd(), non_neg_integer(),
    non_neg_integer(), ranch_transport:sendfile_opts()) ->
    {ok, non_neg_integer()} |
    {error, atom()}.
sendfile(Socket, File, Offset, Bytes, Opts) ->
    ranch_transport:sendfile(?MODULE, Socket, File, Offset, Bytes, Opts).
