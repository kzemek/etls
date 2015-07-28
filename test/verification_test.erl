%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc ssl2 certificate verification tests.
%%%--------------------------------------------------------------------
-module(verification_test).
-author("Konrad Zemek").

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test generators
%%%===================================================================

verification_test_() ->
    {foreach, fun start_application/0, fun clear_queue/1, [
        fun should_accept_self_signed_when_verify_none/1,
        fun should_reject_self_signed/1,
        fun should_accept_expired_certs_when_verify_none/1,
        fun should_reject_expired_certs/1,
        fun should_accept_certificate_signed_by_trusted_CA/1,
        fun should_reject_untrusted_client_certificate/1,
        fun should_accept_client_certificate_signed_by_trusted_CA/1,
        fun should_reject_client_with_no_certificate_when_option_set/1,
        fun should_accept_non_revoked_certificate/1,
        fun should_reject_revoked_certificate/1,
        fun should_accept_using_rfc2818/1,
        fun should_reject_using_rfc2818/1,
        fun should_accept_chained_certificates/1,
        fun should_accept_client_with_proxy_certificate/1,
        fun should_reject_certificates_with_missing_chain/1
    ]}.

%%%===================================================================
%%% Test functions
%%%===================================================================

should_accept_self_signed_when_verify_none(_) ->
    {ok, Acc} = ssl2:listen(5555, server_certs(self_signed)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ok = ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5555, [{verify_type, verify_none}],
            ?TIMEOUT)).

should_reject_self_signed(_) ->
    {ok, Acc} = ssl2:listen(5556, server_certs(self_signed)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:connect("127.0.0.1", 5556, [{verify_type, verify_peer}],
            ?TIMEOUT)).

should_accept_expired_certs_when_verify_none(_) ->
    {ok, Acc} = ssl2:listen(5557, server_certs(expired)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ok = ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5557, [{verify_type, verify_none}],
            ?TIMEOUT)).

should_reject_expired_certs(_) ->
    {ok, Acc} = ssl2:listen(5558, server_certs(expired)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:connect("127.0.0.1", 5558, [{verify_type, verify_peer}],
            ?TIMEOUT)).

should_accept_certificate_signed_by_trusted_CA(_) ->
    {ok, Acc} = ssl2:listen(5559, server_certs(trusted)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ok = ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    {ok, CAFile} = file:read_file("certs/trusted/rootCA.pem"),
    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5559, [{verify_type, verify_peer},
            {cacerts, [CAFile]}], ?TIMEOUT)).

should_reject_untrusted_client_certificate(_) ->
    {ok, Acc} = ssl2:listen(5560, server_certs(trusted) ++ [
        {verify_type, verify_peer}, {fail_if_no_peer_cert, true}]),

    spawn_link(
        fun() ->
            ssl2:connect("127.0.0.1", 5560, client_certs(untrusted), ?TIMEOUT)
        end),

    {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:handshake(SSock, ?TIMEOUT)).

should_accept_client_certificate_signed_by_trusted_CA(_) ->
    {ok, CAFile} = file:read_file("certs/trusted/rootCA.pem"),

    {ok, Acc} = ssl2:listen(5561, server_certs(trusted) ++ [{cacerts, [CAFile]},
        {verify_type, verify_peer}, {fail_if_no_peer_cert, true}]),

    spawn_link(
        fun() ->
            ssl2:connect("127.0.0.1", 5561, client_certs(trusted), ?TIMEOUT)
        end),

    {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
    ?_assertEqual(ok, ssl2:handshake(SSock, ?TIMEOUT)).

should_reject_client_with_no_certificate_when_option_set(_) ->
    {ok, Acc} = ssl2:listen(5562, server_certs(trusted) ++
    [{verify_type, verify_peer}, {fail_if_no_peer_cert, true}]),

    spawn_link(
        fun() ->
            ssl2:connect("127.0.0.1", 5562, [], ?TIMEOUT)
        end),

    {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
    ?_assertEqual({error, 'PEER_DID_NOT_RETURN_A_CERTIFICATE'},
        ssl2:handshake(SSock, ?TIMEOUT)).

should_accept_non_revoked_certificate(_) ->
    {ok, Acc} = ssl2:listen(5563, server_certs(non_revoked)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ok = ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    {ok, CAFile} = file:read_file("certs/non_revoked/rootCA.pem"),
    {ok, CRLFile} = file:read_file("certs/non_revoked/crl.pem"),
    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5563, [{verify_type, verify_peer},
            {cacerts, [CAFile]}, {crls, [CRLFile]}], ?TIMEOUT)).

should_reject_revoked_certificate(_) ->
    {ok, Acc} = ssl2:listen(5564, server_certs(revoked)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    {ok, CAFile} = file:read_file("certs/revoked/rootCA.pem"),
    {ok, CRLFile} = file:read_file("certs/revoked/crl.pem"),
    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:connect("127.0.0.1", 5564, [{verify_type, verify_peer},
            {cacerts, [CAFile]}, {crls, [CRLFile]}], ?TIMEOUT)).

should_accept_using_rfc2818(_) ->
    {ok, Acc} = ssl2:listen(5565, server_certs(trusted)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    {ok, CAFile} = file:read_file("certs/trusted/rootCA.pem"),
    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5565, [{verify_type, verify_peer},
            {cacerts, [CAFile]}, {rfc2818_verification_hostname, "127.0.0.1"}],
            ?TIMEOUT)).

should_reject_using_rfc2818(_) ->
    {ok, Acc} = ssl2:listen(5566, server_certs(trusted)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    {ok, CAFile} = file:read_file("certs/revoked/rootCA.pem"),
    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:connect("127.0.0.1", 5566, [{verify_type, verify_peer},
            {cacerts, [CAFile]}, {rfc2818_verification_hostname, "google.pl"}],
            ?TIMEOUT)).

should_accept_chained_certificates(_) ->
    {ok, RootCA} = file:read_file("certs/chain/rootCA.pem"),
    {ok, SecondCA} = file:read_file("certs/chain/second.pem"),
    {ok, Acc} = ssl2:listen(5567, server_certs(chain) ++ [{chain, [SecondCA]}]),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertMatch({ok, _},
        ssl2:connect("127.0.0.1", 5567, [{verify_type, verify_peer},
            {cacerts, [RootCA]}, {rfc2818_verification_hostname, "127.0.0.1"}],
            ?TIMEOUT)).

should_reject_certificates_with_missing_chain(_) ->
    {ok, RootCA} = file:read_file("certs/chain/rootCA.pem"),
    {ok, Acc} = ssl2:listen(5568, server_certs(chain)),

    Self = self(),
    spawn_link(
        fun() ->
            {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
            ssl2:handshake(SSock, ?TIMEOUT),
            Self ! {ok, SSock}
        end),

    ?_assertEqual({error, 'CERTIFICATE_VERIFY_FAILED'},
        ssl2:connect("127.0.0.1", 5568, [{verify_type, verify_peer},
            {cacerts, [RootCA]}, {rfc2818_verification_hostname, "127.0.0.1"}],
            ?TIMEOUT)).

should_accept_client_with_proxy_certificate(_) ->
    {ok, CAFile} = file:read_file("certs/proxy/rootCA.pem"),
    {ok, Acc} = ssl2:listen(5569, server_certs(trusted) ++ [
        {cacerts, [CAFile]}, {verify_type, verify_peer},
        {fail_if_no_peer_cert, true}]),

    spawn_link(
        fun() ->
            ssl2:connect("127.0.0.1", 5569,
                [{certfile, "certs/proxy/proxy.pem"}], ?TIMEOUT)
        end),

    {ok, SSock} = ssl2:accept(Acc, ?TIMEOUT),
    ?_assertEqual(ok, ssl2:handshake(SSock, ?TIMEOUT)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start_application() ->
    ssl2_app:start(temporary, []).

clear_queue(_) ->
    receive
        _ -> clear_queue([])
    after 0 ->
        ok
    end.

%%%===================================================================
%%% Helper functions
%%%===================================================================

server_certs(Type) ->
    certs(Type, server).

client_certs(Type) ->
    certs(Type, client).

certs(Type, Who) ->
    [{certfile, filename:join(
        ["certs", atom_to_list(Type), atom_to_list(Who) ++ ".pem"])},
        {keyfile, filename:join(
            ["certs", atom_to_list(Type), atom_to_list(Who) ++ ".key"])}].
