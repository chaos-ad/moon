-module(moon_nif).

-export([start/1, call/3, result/2]).
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_) ->
    exit(nif_library_not_loaded).

call(_, _, _) ->
    exit(nif_library_not_loaded).

result(_, _) ->
    exit(nif_library_not_loaded).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions:

init() ->
    {ok, CWD} = file:get_cwd(),
    NifPath = filename:join(CWD, "priv/moon_nif"),
    ok = erlang:load_nif(NifPath, 0).
