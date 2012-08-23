-module(moon).

-export([init/0, open/0, close/1, call/2]).
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    {ok, CWD} = file:get_cwd(),
    NifPath = filename:join(CWD, "priv/moon"),
    ok = erlang:load_nif(NifPath, 0).

open() ->
    exit(nif_library_not_loaded).

close(_) ->
    exit(nif_library_not_loaded).

call(_, _) ->
    exit(nif_library_not_loaded).
