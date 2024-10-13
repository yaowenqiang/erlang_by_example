-module(echo).
-export([echo/0]).

echo() -> 
    receive
            {Pid, Msg} ->
                Pid ! Msg,
            echo()
    end.


