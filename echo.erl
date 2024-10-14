-module(echo).
-export([echo/0, launch/0]).

launch() ->
    register(echo, spawn(echo, echo, [])).

echo() -> 
    receive
            {Pid, Msg} ->
                Pid ! Msg,
            echo()
    end.


