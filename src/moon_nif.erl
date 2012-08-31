-module(moon_nif).

-export([start/1, load/2, eval/2, call/3, result/2]).
-on_load(init/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_) ->
    exit(nif_library_not_loaded).

load(_, _) ->
    exit(nif_library_not_loaded).

eval(_, _) ->
    exit(nif_library_not_loaded).

call(_, _, _) ->
    exit(nif_library_not_loaded).

result(_, _) ->
    exit(nif_library_not_loaded).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions:

init() ->
    SoName = filename:join(priv_dir_path(moon), ?MODULE),
    ok = erlang:load_nif(filename:absname(SoName), 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

priv_dir_path(App) ->
    case code:priv_dir(App) of
        {error, bad_name} -> priv_dir_mod(App);
        Dir -> Dir
    end.

priv_dir_mod(Mod) ->
    case code:which(Mod) of
        File when not is_list(File) -> "../priv";
        File -> filename:join([filename:dirname(File),"../priv"])
    end.
