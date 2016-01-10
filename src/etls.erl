-module('etls').

-record(etls_handle, {
  socket :: gen_tcp:socket(),
  ssl :: etls_nif:ssl_handle(),
  input_bio :: etls_nif:bio_handle(),
  output_bio :: etls_nif:bio_handle()
}).

-opaque sslsocket() :: #etls_handle{}.

%% API exports
-export_type([sslsocket/0]).
-export([connect/2, connect/3, send/2, recv/2, recv/3]).

%%====================================================================
%% API functions
%%====================================================================

connect(Socket, SslOptions) ->
  connect(Socket, SslOptions, infinity).

connect(Socket, SslOptions, Timeout) ->
  inet:setopts(Socket, [{active, false}, binary]),
  SSL = etls_nif:ssl_new(),
  InputBio = etls_nif:bio_new(),
  OutputBio = etls_nif:bio_new(),

  etls_nif:ssl_set_bio(SSL, InputBio, OutputBio),
  etls_nif:ssl_set_connect_state(SSL),

  Handle = #etls_handle{
    ssl = SSL,
    socket = Socket,
    input_bio = InputBio,
    output_bio = OutputBio},

  case handshake(Handle, Timeout) of
    ok -> {ok, Handle};
    Other -> Other
  end.

send(Socket, Data) when not is_binary(Data) ->
  send(Socket, iolist_to_binary(Data));
send(Socket, Data) ->
  #etls_handle{ssl = SSL} = Socket,
  ToWrite = byte_size(Data),
  case etls_nif:ssl_write(SSL, Data) of
    ToWrite -> push_data(Socket);
    RetCode ->
      case handle_error(Socket, RetCode, infinity) of
        ok -> send(Socket, Data);
        Other -> Other
      end
  end.

recv(Socket, Size) ->
  recv(Socket, Size, infinity).

recv(Socket, Size, Timeout) ->
  recv(Socket, Size, Timeout, []).

%%====================================================================
%% Internal functions
%%====================================================================

recv(Socket, Size, Timeout, Acc) ->
  #etls_handle{ssl = SSL} = Socket,
  ReqSize = case Size of 0 -> 1024; _ -> Size end,
  case etls_nif:ssl_read(SSL, ReqSize) of
    <<Data/binary>> ->
      case Size of
        0 -> {ok, Data};
        _ ->
          Left = Size - byte_size(Data),
          case Left > 0 of
            true -> recv(Socket, Left, Timeout, [Data | Acc]);
            false -> {ok, iolist_to_binary(lists:reverse([Data | Acc]))}
          end
      end;

    RetCode ->
      case handle_error(Socket, RetCode, Timeout) of
        ok -> recv(Socket, Size, Timeout, Acc);
        Other -> Other
      end
  end.

handshake(Handle, Timeout) ->
  #etls_handle{ssl = SSL} = Handle,
  case etls_nif:ssl_do_handshake(SSL) of
    1 -> push_data(Handle);
    RetCode ->
      case handle_error(Handle, RetCode, Timeout) of
        ok -> handshake(Handle, Timeout);
        Other -> Other
      end
  end.

handle_error(Handle, RetCode, Timeout) ->
  case etls_nif:ssl_get_error(Handle#etls_handle.ssl, RetCode) of
    'SSL_ERROR_WANT_READ' ->
      push_data(Handle),
      get_data(Handle, Timeout);

    'SSL_ERROR_WANT_WRITE' ->
      push_data(Handle);

    'SSL_ERROR_ZERO_RETURN' ->
      {error, closed};

    Error -> {error, Error}
  end.

get_data(Handle, Timeout) ->
  #etls_handle{input_bio = InputBio, socket = Socket} = Handle,
  case gen_tcp:recv(Socket, 0, Timeout) of
    {ok, Data} ->
      DataSize = byte_size(Data),
      case etls_nif:bio_write(InputBio, Data) of
        DataSize -> ok;
        _ -> {error, bio_write_error}
      end;

    Error -> Error
  end.

push_data(Handle) ->
  #etls_handle{output_bio = OutputBio, socket = Socket} = Handle,
  case etls_nif:bio_read(OutputBio) of
    <<>> -> ok;
    <<Data/binary>> -> gen_tcp:send(Socket, Data);
    _ -> {error, bio_read_error}
  end.