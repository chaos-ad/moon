-module(moon_vm).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/0, start_link/1, call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

call(Pid, Fun, Args) ->
    gen_server:call(Pid, {call, Fun, Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, VM} = moon_nif:start(self()),
    {ok, VM}.

handle_call({call, Fun, Args}, _, VM) ->
    ok = moon_nif:call(VM, Fun, Args),
    {reply, receive_response(VM), VM}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_response(VM) ->
    receive
        {VM, response, Response} ->
            {reply, Response, VM};
        {VM, callback, {M, F, A}} ->
            moon_nif:callback_result(VM, catch erlang:apply(M,F,A)),
            receive_response(VM)
    end.
