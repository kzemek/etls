%%%-------------------------------------------------------------------
%%% @author konrad
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2016 15:04
%%%-------------------------------------------------------------------
-module(etls_nif).
-author("konrad").

-on_load(init/0).

-type bio_handle() :: term().
-type ssl_handle() :: term().

-export_type([bio_handle/0, ssl_handle/0]).

%% API
-export([bio_new/0, bio_read/1, bio_write/2, ssl_new/0, ssl_get_error/2,
  ssl_set_connect_state/1, ssl_do_handshake/1, ssl_set_bio/3, ssl_write/2,
  ssl_read/2, ssl_set_accept_state/1, ssl_use_certificate_file/2,
  ssl_use_privatekey_file/2]).

-spec bio_new() -> bio_handle().
bio_new() ->
  erlang:nif_error(etls_bio_nif_not_loaded).

-spec bio_read(bio_handle()) -> binary() | integer().
bio_read(_BIO) ->
  erlang:nif_error(etls_bio_nif_not_loaded).

-spec bio_write(bio_handle(), binary()) -> integer().
bio_write(_BIO, _Data) ->
  erlang:nif_error(etls_bio_nif_not_loaded).

-spec ssl_new() -> ssl_handle().
ssl_new() ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_get_error(ssl_handle(), integer()) -> atom().
ssl_get_error(_SSL, _RetCode) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_set_bio(ssl_handle(), bio_handle(), bio_handle()) -> ok.
ssl_set_bio(_SSL, _RBio, _WBio) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_set_connect_state(ssl_handle()) -> ok.
ssl_set_connect_state(_SSL) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_set_accept_state(ssl_handle()) -> ok.
ssl_set_accept_state(_SSL) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_do_handshake(ssl_handle()) -> integer().
ssl_do_handshake(_SSL) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_write(ssl_handle(), iodata()) -> integer().
ssl_write(_SSL, _Data) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_read(ssl_handle(), non_neg_integer()) -> binary() | integer().
ssl_read(_SSL, _Num) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_use_certificate_file(ssl_handle(), string()) -> integer().
ssl_use_certificate_file(_SSL, _FileName) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec ssl_use_privatekey_file(ssl_handle(), string()) -> integer().
ssl_use_privatekey_file(_SSL, _FileName) ->
  erlang:nif_error(etls_ssl_nif_not_loaded).

-spec init() -> ok | {error, Reason :: atom()}.
init() ->
  LibName = "libetls_nif",
  LibPath =
    case code:priv_dir(etls) of
      {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", priv])) of
          true ->
            filename:join(["..", priv, LibName]);
          _ ->
            filename:join([priv, LibName])
        end;

      Dir ->
        filename:join(Dir, LibName)
    end,

  erlang:load_nif(LibPath, 0).
