# Moon

Library for calling Lua from Erlang, and back.

## Dependencies:

Parts of the boost library, somewhat close to 1.48  
Developement version of lua

These libraries easily can be obtained on ubuntu by running this:

`
sudo apt-get install libboost1.48-all-dev liblua5.2-dev
`

## General usage:

Here an self-describing example of usage:

    {ok, VM} = moon:start_vm(). %% Beware! It spawns the os thread.
    {ok,{17,<<"hello">>}} = moon:eval(VM, "return 17, \"hello\"").
    ok = moon:load(VM, "priv/test.lua").
    moon:call(VM, test_function, [first_arg, <<"second_arg">>, [{key, val}, {key2, val2}]]).
    ok = moon:stop_vm(VM).

Strictly speaking, moon:stop_vm/1 is used here just for symmetry.
VM will be stopped and freed when the erlang garbage collector detects that VM become a garbage.

## Callbacks from lua to erlang:

There is two possible modes in which callbacks are handled:

#### Permissive mode
This mode allows to invoke arbitrary function from lua with erlang.call(Module, Function, Args):

    {ok, VM} = moon:start_vm().
    {ok, <<"ok">>} = moon:eval(VM, <<"return erlang.call(\"io\", \"format\", {\"Look ma, im calling! ~s~n\", {\"Yay\"}}).result">>).

    Though, it is not very useful, since type mapping is far from done, and there is no way
    to construct atoms or strings from lua.

#### Restrictive mode
This mode passes all calls to erlang.call from lua to a single callback.
Dispatching and/or type mapping can be done here.

    Callback = fun(X) -> io:format("Callback called; args: ~p~n", [X]) end.
    {ok, VM} = moon:start_vm([{callback, Callback}]).
    {ok, <<"ok">>} = moon:eval(VM, "return erlang.call({\"hello\"}).result").

## Type mapping:

<table>
  <tr>
    <th>Erlang</th>
    <th>Lua</th>
    <th>Remarks</th>
  </tr>
  <tr>
    <td>42</td>
    <td>42</td>
    <td></td>
  </tr>
  <tr>
    <td>true</td>
    <td>true</td>
    <td>boolean in lua</td>
  </tr>
  <tr>
    <td>false</td>
    <td>false</td>
    <td>boolean in lua</td>
  </tr>
  <tr>
    <td>atom</td>
    <td>"atom"</td>
    <td>string in lua</td>
  </tr>
  <tr>
    <td>"string"</td>
    <td>{'s', 't', 'r', 'i', 'n', 'g'}</td>
    <td>table in lua, don't use it!</td>
  </tr>
  <tr>
    <td><<"binary">></td>
    <td>"binary"</td>
    <td>string in lua</td>
  </tr>
  <tr>
    <td>[]</td>
    <td>{}</td>
    <td></td>
  </tr>
  <tr>
    <td>[10, 100, <<"abc">>]</td>
    <td>{10, 100, "abc"}</td>
    <td></td>
  </tr>
  <tr>
    <td>[{yet, value}, {another, value}]</td>
    <td>{yet="value", another="value"}</td>
  </tr>
  <tr>
    <td>[{ugly, "mixed"}, list]</td>
    <td>{ugly="mixed", "list"}</td>
    <td>list will be accessable at index [1], and "mixed" under the key "ugly"</td>
  </tr>
</table>

## Todo:
* Get rid of libboost_thread dependency, and replace queue with just a mutex & condition variable
* Embed header-only part of boost to the build
* Convert erlang strings to lua strings properly
