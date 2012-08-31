-module(moon_vm).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/0, start_link/1]).
-export([load/3, eval/3, call/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

load(Pid, File, Timeout) ->
    gen_server:call(Pid, {load, File}, Timeout).

eval(Pid, Code, Timeout) ->
    gen_server:call(Pid, {eval, Code}, Timeout).

call(Pid, Fun, Args, Timeout) ->
    gen_server:call(Pid, {call, Fun, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, VM} = moon_nif:start(self()),
    {ok, VM}.

handle_call({load, File}, _, VM) ->
    ok = moon_nif:load(VM, to_binary(File)),
    {reply, receive_response(VM), VM};

handle_call({eval, Code}, _, VM) ->
    ok = moon_nif:eval(VM, to_binary(Code)),
    {reply, receive_response(VM), VM};

handle_call({call, Fun, Args}, _, VM) when is_list(Args) ->
    ok = moon_nif:call(VM, to_atom(Fun), Args),
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
        {moon_response, Response} ->
            Response;
        {moon_callback, {M,F,A}} ->
            try erlang:apply(to_atom(M),to_atom(F),A) of
                Result ->
                    moon_nif:result(VM, [{error, false}, {result, Result}]),
                    receive_response(VM)
            catch
                _:Error ->
                    moon_nif:result(VM, [{error, true}, {result, Error}]),
                    receive_response(VM)
            end;
        {moon_callback, _} ->
            moon_nif:result(VM, [{error, true}, {result, invalid_call}]),
            receive_response(VM);
        Other ->
            error({invalid_response, Other})
    end.

to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) -> list_to_atom(Val);
to_atom(Val) when is_binary(Val) -> list_to_atom(binary_to_list(Val)).

to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> list_to_binary(atom_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val).
