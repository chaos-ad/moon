-module(moon_vm).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/1]).
-export([load/3, eval/3, call/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public api:

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

load(Pid, File, Timeout) ->
    gen_server:call(Pid, {load, File}, Timeout).

eval(Pid, Code, Timeout) ->
    gen_server:call(Pid, {eval, Code}, Timeout).

call(Pid, Fun, Args, Timeout) ->
    gen_server:call(Pid, {call, Fun, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private api:

-record(state, {vm, callback}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Options) ->
    Callback = proplists:get_value(callback, Options),
    {ok, VM} = moon_nif:start(self()),
    {ok, #state{vm=VM, callback=Callback}}.

handle_call({load, File}, _, State=#state{vm=VM}) ->
    ok = moon_nif:load(VM, to_binary(File)),
    {reply, receive_response(State), State};

handle_call({eval, Code}, _, State=#state{vm=VM}) ->
    ok = moon_nif:eval(VM, to_binary(Code)),
    {reply, receive_response(State), State};

handle_call({call, Fun, Args}, _, State=#state{vm=VM}) when is_list(Args) ->
    ok = moon_nif:call(VM, to_atom(Fun), Args),
    {reply, receive_response(State), State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_response(State=#state{vm=VM, callback=Callback}) ->
    receive
        {moon_response, Response} ->
            Response;
        {moon_callback, Args} ->
            try
                case handle_callback(Callback, Args) of
                    {error, Result} -> moon_nif:result(VM, [{error, true}, {result, Result}]);
                    {ok, Result}    -> moon_nif:result(VM, [{error, false}, {result, Result}]);
                    Result          -> moon_nif:result(VM, [{error, false}, {result, Result}])
                end
            catch _:Error ->
                moon_nif:result(VM, [{error, true}, {result, Error}])
            end,
            receive_response(State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_callback(undefined, {Mod, Fun, Args}) ->
    erlang:apply(to_atom(Mod),to_atom(Fun),Args);

handle_callback(Callback, Args) when is_function(Callback) ->
    Callback(Args);

handle_callback({Mod, Fun}, Args) when is_atom(Mod), is_atom(Fun) ->
    erlang:apply(Mod, Fun, Args);

handle_callback({Mod, Fun, Args0}, Args1)
        when is_atom(Mod), is_atom(Fun)
           , is_list(Args0), is_list(Args1) ->
    erlang:apply(Mod, Fun, Args0 ++ Args1);

handle_callback(_, _) ->
    error(invalid_call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) -> list_to_atom(Val);
to_atom(Val) when is_binary(Val) -> list_to_atom(binary_to_list(Val)).

to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> list_to_binary(atom_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val).
