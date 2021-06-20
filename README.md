# iconv

[![CI](https://github.com/processone/iconv/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/processone/iconv/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/processone/iconv/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/iconv?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/iconv.svg "Hex version")](https://hex.pm/packages/iconv)

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
$ erl -pa ebin -pa deps/*/ebin
Erlang/OTP 21 [erts-10.2.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V10.2.1  (abort with ^G)
1> application:start(iconv).
ok
2> FromEncoding = <<"utf-8">>. 
<<"utf-8">>
3> ToEncoding = <<"iso8859-15">>.
<<"iso8859-15">>
4> Text = <<"Hello">>.
<<"Hello">>
5> iconv:convert(FromEncoding, ToEncoding, Text).            
<<"Hello">>
```

## Elixir

You can use `iconv` with Elixir `mix` by adding the dependency as follows:

```
  defp deps do
    [
      {:iconv, "~> 1.0.10"},
    ]
  end
```

```
$ mix deps.get
...
$ mix deps.compile
...
$ iex -S mix
Erlang/OTP 21 [erts-10.2.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Interactive Elixir (1.7.4) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> :iconv.convert("utf-8", "iso8859-15", "Hello")
"Hello"
```

## Development

### Test

#### Unit test

You can run eunit test with the command:

    $ rebar eunit

