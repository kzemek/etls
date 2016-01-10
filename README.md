# etls

An OTP library

## Why etls?

Because speed, mostly because hardware acceleration.

## Why BoringSSL instead of OpenSSL?

blog

## How do I use it?

## Why is <feature X> not implemented?

Because I didn't need it. Feel free to submit a feature request!

## Build requirements

* [Erlang] >= 18.0 with dirty schedulers enabled
* [CMake] >= 3.0
* Perl >= 5.6.1
* [Ninja]
* [Go]
* C++14-compatible compiler

## Build

```
$ ./rebar3 compile
```

[Erlang]: http://www.erlang.org/download.html
[Ninja]: https://martine.github.io/ninja/
[CMake]: http://www.cmake.org/download/
[Go]: https://golang.org/dl/