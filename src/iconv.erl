%%%----------------------------------------------------------------------
%%% File    : iconv.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to libiconv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%----------------------------------------------------------------------

-module(iconv).

-author('alexey@process-one.net').

-compile(no_native).

-export([load_nif/0, load_nif/1, convert/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================
load_nif() ->
    NifFile = p1_nif_utils:get_so_path(?MODULE, [iconv], "iconv"),
    case erlang:load_nif(NifFile, 0) of
        ok ->
            ok;
        {error, {Reason, Txt}} ->
            error_logger:error_msg("failed to load NIF ~s: ~s",
                                   [NifFile, Txt]),
            {error, Reason}
    end.

load_nif(_LibDir) ->
    load_nif().

-spec convert(iodata(), iodata(), iodata()) -> binary().

convert(_From, _To, _String) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).

load_nif_test() ->
    ?assertEqual(ok, load_nif(filename:join(["..", "priv", "lib"]))).

utf8_to_koi8r_test() ->
    ?assertEqual(
       <<212,197,211,212>>,
       iconv:convert("utf-8", "koi8-r", <<209,130,208,181,209,129,209,130>>)).

koi8r_to_cp1251_test() ->
    ?assertEqual(
       <<242,229,241,242>>,
       iconv:convert("koi8-r", "cp1251", <<212,197,211,212>>)).

wrong_encoding_test() ->
    ?assertEqual(
       <<1,2,3,4,5>>,
       iconv:convert("wrong_encoding_from",
                     "wrong_encoding_to",
                     <<1,2,3,4,5>>)).

-endif.
