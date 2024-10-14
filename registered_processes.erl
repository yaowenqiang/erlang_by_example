-module(registered_processes).
-export([go/0, loop/0]).

go() -> register(registered_processes, spawn(registered_processes, loop, [])).

loop() -> 
    receive
        {From ,Msg} -> 
            From ! {self(), Msg},
            loop();
        stop -> true
    end.
