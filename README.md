# etls

[![Build Status](https://travis-ci.org/kzemek/etls.svg?branch=master)](https://travis-ci.org/kzemek/etls)
[![Hex.pm](https://img.shields.io/hexpm/v/etls.svg?maxAge=2592000)](https://hex.pm/packages/etls)
[![Hexdocs.pm](https://img.shields.io/badge/api-hexdocs-brightgreen.svg)](https://hexdocs.pm/etls)

An alternative implementation of Erlang TCP/TLS layer.

*etls* is a NIF-based implementation of the whole TLS stack, built on top of
[Asio] and [BoringSSL]. It manages its own native threads to asynchronously
handle socket operations.

The main (and very important) benefit of using this project instead of Erlang's
built-in [`ssl`] is hardware acceleration. `etls` module achieves an order of
magnitude higher bandwidth when encoding/decoding data.

Currently only `TLSv1.2` is supported, and default [BoringSSL] cipher is used.

## Performance

Benchmark ran on OS X 10.11.5, 2,2 GHz Intel Core i7 (4 cores with HT). The
benchmark consisted of **10 concurrent connections**, each sending **10
messages**, each of **size 100 MB** for a total of **1 GB per connection**. The
bandwidth has been calculated using time measured between the first message sent
and the last message received.

| OTP version | transport | bandwidth |
|:------------|:----------|:----------|
| 18.3        | ssl       | 70 MB/s   |
| 19.0-rc1    | ssl       | 111 MB/s  |
| 19.0-rc1    | etls      | 833 MB/s  |

## Build

Dependencies:

* `cmake` >= 3.1.0
* `erlang` >= 17.0
* `g++` >= 4.9.0 (or `clang`)
* `git`
* `perl`
* `golang`
* `make`
* `ninja-build`
* `openssl`

To build the project, simply run `make` from its directory.

# User Guide

Add `etls` as a `rebar` dependency to your project:

```erlang
{deps, [
  {etls, "1.1.2", {git, "https://github.com/kzemek/etls.git", {tag, "1.1.2"}}}
}.
```

You can also take advantage of the [hex.pm package].

Now you can use `etls` module much like you would use `ssl`:

```erlang
% Server side
application:start(etls),

{ok, ListenSocket} =
  etls:listen(9999, [{certfile, "cert.pem"}, {keyfile, "key.pem"},
                     {reuseaddr, true}]),

{ok, Socket} = etls:accept(ListenSocket),
etls:handshake(Socket),
etls:setopts(Socket, [{active, once}]),

receive AMessage -> io:format("~p~n", [AMessage]) end.
```


```erlang
% Client side
application:start(etls),

{ok, Socket} = etls:connect("localhost", 9999,  [], infinity),
etls:send(Socket, "foo").
```

## Using with Ranch

`etls` can be easily used with [Ranch] by [starting a
listener](http://ninenines.eu/docs/en/ranch/1.2/guide/listeners/) with
`ranch_etls` as the transport module:

```erlang
{ok, _} = ranch:start_listener(tcp_echo, 100,
                               ranch_etls, [{port, 5555}, {certfile, CertPath}],
                               echo_protocol, []).
```

## APIs

API documentation can be found at [hexdocs.pm].

### Implemented `ssl` functions

The following `ssl`/[`inet`] functions are currently implemented:

* `connect/3`
* `connect/4`
* `send/2`
* `recv/2`
* `recv/3`
* `listen/2`
* `accept/1` (`ssl`: `transport_accept/1`)
* `accept/2` (`ssl`: `transport_accept/2`)
* `handshake/1` (`ssl`: `accept/1`)
* `handshake/2` (`ssl`: `accept/2`)
* `setopts/2`
* `controlling_process/2`
* `peername/1`
* `sockname/1`
* `close/1`
* `peercert/1`
* `certificate_chain/1` (not present in `ssl`)
* `shutdown/2`

### Implemented `ssl` options

The following `ssl`/`inet` options are currently supported:

* `{packet, raw | 0 | 1 | 2 | 4}`
* `{active, boolean() | once}`
* `{exit_on_close, boolean()}`
* `{verify_type, verify_none | verify_peer}`
* `{fail_if_no_peer_cert, boolean()}`
* `{verify_client_once, boolean()}`
* `{rfc2818_verification_hostname, str()}`
* `{cacerts, [pem_encoded()]}`
* `{crls, [pem_encoded()]}`
* `{certfile, str()}`
* `{keyfile, str()}`
* `{chain, [pem_encoded()]}`

[Asio]: http://think-async.com/
[BoringSSL]: https://boringssl.googlesource.com/boringssl/
[Ranch]: https://github.com/ninenines/ranch
[`ssl`]: http://erlang.org/doc/man/ssl.html
[`inet`]: http://erlang.org/doc/man/inet.html
[hex.pm package]: https://hex.pm/packages/etls/
[hexdocs.pm]: https://hexdocs.pm/etls/
