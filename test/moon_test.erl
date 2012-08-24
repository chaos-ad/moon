-module(moon_test).
-include_lib("eunit/include/eunit.hrl").

the_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"Starting/Stopping the VM",
                fun() ->
                    Res = moon:start_vm(),
                    ?assertMatch({ok,_}, Res),
                    ?assertEqual(ok, moon:stop_vm(element(2,Res)))
                end
            }
        ]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
    error_logger:tty(false),
    application:start(moon).

teardown(_) ->
    application:stop(moon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
