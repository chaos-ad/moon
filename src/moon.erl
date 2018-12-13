-module(moon).

-export([start/0, stop/0]).
-export([start_vm/0, start_vm/1, stop_vm/1]).

-export([load/2, load/3]).
-export([eval/2, eval/3]).
-export([call/3, call/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(?MODULE).

start(App) ->
    start_ok(App, application:start(App, permanent)).

stop() ->
    application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_ok(_, ok) ->
    ok;

start_ok(_, {error, {already_started, _App}}) ->
    ok;

start_ok(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = start(Dep),
    start(App);

start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_vm() ->
    start_vm([]).

start_vm(Options) ->
    moon_sup:start_child([Options]).

stop_vm(Pid) ->
    moon_sup:stop_child(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(Pid, File) ->
    load(Pid, File, infinity).

load(Pid, File, Timeout) ->
    case moon_vm:load(Pid, File, Timeout) of
        ok -> ok;
        {ok, Info} ->
            {ok, Info};
        {error, ErrInfo} ->
            {error, handle_error(ErrInfo)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval(Pid, Code) ->
    eval(Pid, Code, infinity).

eval(Pid, Code, Timeout) ->
    case moon_vm:eval(Pid, Code, Timeout) of
        ok -> ok;
        {ok, Info} ->
            {ok, Info};
        {error, ErrInfo} ->
            {error, handle_error(ErrInfo)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call(Pid, Fun, Args) ->
    call(Pid, Fun, Args, infinity).

call(Pid, Fun, Args, Timeout) ->
    case moon_vm:call(Pid, Fun, Args, Timeout) of
        ok -> ok;
        {ok, Info} ->
            {ok, Info};
        {error, ErrInfo} ->
            {error, handle_error(ErrInfo)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% return error msg
handle_error(Err) when erlang:is_tuple(Err)  ->
    concat_error(lists:reverse(erlang:tuple_to_list(Err)), []);
handle_error(Err)  ->
    %% nil function ...
    Err.

%% contxt all error msg to one
concat_error([Text | Tils], Rtn) when erlang:is_tuple(Text) ->
    NewRtn = lists:concat([Rtn | concat_error([], erlang:tuple_to_list(Text))]),
    concat_error(Tils, NewRtn);
%% handle error msg
concat_error([Text | Tils], Rtn) when erlang:is_bitstring(Text) ->
    NewText = re:replace(Text, "\n", "~n", [{return, list}, global]),
    %% c "\t" == 4 space
    NewText1 = re:replace(NewText, "\t", "    ", [{return, list}, global]),
    concat_error([NewText1 | Tils], Rtn);
concat_error([Text | Tils], Rtn) ->
    NewRtn = lists:concat([Rtn, "~n", Text]),
    concat_error(Tils, NewRtn);
concat_error([], Err) ->
    Err.