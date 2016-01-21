# iconv

Fast encoding conversion library for Erlang / Elixir

This library is a native binding to
[libiconv](https://www.gnu.org/software/libiconv/) library.

## Building

iconv library can be build as follow:

    ./configure && make

iconv is a rebar-compatible OTP application. Alternatively, you can
build it with rebar:

    rebar compile

## Dependencies

iconv library depends on libiconv.

You can use `configure` option to pass custom path to libiconv
library:

    --with-libiconv-prefix[=DIR]  search for libiconv in DIR/include and DIR/lib

## Usage

You can start iconv with the following command:

```shell
$ erl -pa ebin
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)

% Start the application:
1> application:start(iconv).
```

## Development

### Test

#### Unit test

You can run eunit test with the command:

    $ rebar eunit


