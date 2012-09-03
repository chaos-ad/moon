# Moon

Library for calling Lua from Erlang, and back.

## Usage:

Here an self-describing example of usage:

    {ok, VM} = moon:start_vm(). %% Beware! It spawns the os thread.
    {ok,{17,<<"hello">>}} = moon:eval(VM, "return 17, \"hello\"").
    ok = moon:load(VM, "priv/test.lua").
    moon:call(VM, test_function, [first_arg, <<"second_arg">>, [{key, val}, {key2, val2}]]).
    ok = moon:stop_vm(VM).

Strictly speaking, moon:stop_vm/1 is used here just for symmetry.
VM will be stopped and freed when the erlang garbage collector detects that VM become a garbage.

## TODO
Get rid of libboost_thread dependency, and replace queue with just a mutex & condition variable
