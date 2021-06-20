-module(unload_test).
-export([unload_test/0]).

unload_test() ->
    code:delete(fast_yaml),
    code:purge(fast_yaml),
    code:load_file(fast_yaml).
