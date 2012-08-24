-module(moon_sup).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, start_child/0, stop_child/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args, Role), {I, {I, start_link, Args}, permanent, 60000, Role, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->

    %% To get an error immideately, if there some troubles with nif
    code:ensure_loaded(moon_nif),

    {ok, { {simple_one_for_one, 5, 10}, [
        {moon_vm, {moon_vm, start_link, []}, temporary, 60000, worker, [moon_vm]}
    ]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
