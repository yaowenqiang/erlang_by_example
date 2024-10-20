-module(frequency).
-export([start/0, init/0, loop/1, get_frequencies/0, allocate/2]).

start() -> 
    register(frequency, spawn(?MODULE, init, [])).
init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, deallocate, Freq} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {"EXIT", Pid, _Reason} ->
            NewFrequencies = exited(Frequencies, Pid),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok),
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

% allocate({[], Allocated}, _Pid) ->
%     {{[], Allocated},
%         {error, no_frequency} }.

allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),
    {{Free, [{Freq, Pid}|Allocated]},{ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    {value, {Freq, Pid}} = lists:keysearch(Freq, 1, Allocated),
    unlink(Pid),
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

% Need to handle "EXIT" message, we use the exited funciton
% Need to link on allocation
% Need to unlink on deallocation

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid, 2, Allocated) of
        {value, {Freq, Pid}} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {Free, NewAllocated};
        false ->
            {Free, Allocated}
    end.


% exited is called to clean up if exit is before deallocation
% in exited/2 check that {Freq, Pid} is member of the frequency list
% Avoids a race condition when client deallocates frequency but terminates before the server can handle the deallocate message
% Use bidirectional link: want clients to terminate if the frequency server terminates