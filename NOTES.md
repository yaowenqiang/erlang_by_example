# Erlang by example


## Telecom Applications: issues

+ Complex
+ No down time
+ Scalable
+ Maintainable
+ Distributed
+ Time to Market

## Erlang Properties

+ Declarative - Function Programming language, high abstraction level, pattern matching and concise readable programs
+ Concurrency - Elther transparent or explicit Concurrency, lightweight processes and highly Scalable
+ Soft Real-Time - Response times in the order of milliseconds per process garbage collection
+ Robustness - Simple and consistent error recovery, supervision hierarchies and "Program for the correct case"
+ Distribution - Explicit or transparent distribution. Network-aware runtime system
+ Hot code loading - Easily change code in a running system. Enables non-stop operations, Simplifies testing
+ External interfaces - "Port" to the outside world behave as Erlang processes
+ Portability - Erlang runs on any UNIX, UNIX-like, Windows, OSX, Android, VX works, supports heterogenous networks
+ SMP supports - Symmetric multiprocessing support. Takes full advantages of multicore archihectures

## Data Types: integers, floats and atoms

+ 10 
+ -234
+ 16#AB!0F 
+ $A
+ $\n
+ 17.360
+ -56.564
+ 12.34E-10

+ alpha12
+ start_with_lower_case
+ true, false
+ 'January'
+ 'a space'

+ Large integers are converted to bignums
+ B#Val numbers in base B 
+ $Char for ASCII values
+ IEEE 754 floating point
+ Atoms are constant iterals
+ Start with a lower case letter or are encapsulated by ''

## Data Types: tuples and lists

+ {123, abc}
+ {abc, {def ,123}, ghi}
+ {person, 'Joe', 'Armstrong'}
+ [january, february, march]
+ [a,[b,[c,d,e], f], g]
+ []
+ [{person, 'Joe', 'Armstrong'}, {person, 'Robert', 'Virding'}]
+ [72, 101, 100, 10008, 111]
+ [$H ,$e ,$l, $o]
+ "Hello world"

+ Each used for collection of values ...
+ ... they differ in how you can access and program withem
+ Tuples are used to denote data-types with a fixed number of items
+ Lists are used to store a variable number of items 
+ String are list if ASCII codes.

## Variables

+ A_long_variable_name
+ Flag
+ Name2
+ DbgFlag
+ _a_do_not_cale_variable
+ _

> variable can only be bound once

+ Variables can start with an uppercase letter or _
+ No 'funny characters'
+ _ alone is a don't care variable
+ Single assignment
+ No need to declare them
+ Typing are determined at run time

## Pattern matching

> Pattenr = Expression


+ Pattern matching is used for:
  + Assigning values to Variables
  + Controlling the execution flow of progrmas(if, case, function heads)
  + Extracing values from compound data Types
  + The pattern can contain variables which are bound when the matching succeeds
  + The Expression may not contain unbound variables


```erlang
{E, E, foo} = {abc, abc, foo}
[H|T] = [1,2,3]
f() 
```

+ A = 10
+ {B,C,D} = {10, foo, bar}
+ {E, E, foo} = {abc, abc, foo}
+ [H|T] = [1,2,3]
+ {A, A, B} = [abc, def ,123] x .. or fail, binding none
+ [H|T] = []  
+ {A, _, [B|_], {B}} = {abc, 23, [22, x], {22}}
+ C = 10, {C ,C ,3} = {10, 10, 3} - Selection of components from complex data structures

## Functions : syntax

```erlang
area({square, Side}) -> 
    Side * Side;
area({circle, Radius}) -> 
    3.1416 * Radius * Radius;
area({triangle, A,B,C}) -> 
    S = (A + B + C) / 2,
    math:sqrt(S*(S-A)*(S-b)*(S-C));
area(Other) -> 
    {error, invalid_object}

```

### Function examples

```erlang
factorial(0) -> 1;
factorial(N) -> 
    N * factorial(N-1).
```

## Modules

```erlang
-module(demo).
-export([double/1]).
% This is a comment
% Everything after %' is ignored

double(X) -> 
    times(X, 2)

times(X, N) -> 
    X * N.
```


### Conditional Evaluation: case


```erlang
case lists:members(elementValueToTest, ListNameVariable) of 
    true -> ok;
    false -> {error, unknown}
end

```

### Conditional Evaluation: if

```erlang
if
    X <1 -> smaller;
    X >1 -> bigger;
    X ==1 -> equal
end

```


## Get help in the erl(shell)

```bash
h() 
h(lists)
b()

```

### Guards


```erlang
factorial(N) when N > 0 -> 
    N * factorial(N-1);
factorial(0) -> 1.

% This is not the same as

factorial(0) -> 1;
factorial(N) -> 
    N * factorial(N-1).

```

+ The reserved word when introduced a guard
+ Guards can be used in function heads, case clauses, receive and if expressions
+ ==, =:=, <, >, etc
+ is_number(X), is_integer(X), is_float(X)
+ - X is a number
+ is_atom(X), is_tuple(X), etc

### Build-in Functions

+ date()
+ time()
+ length(List)
+ size(Tuple)
+ atom_to_list(Atom)
+ list_to_tuple(List)
+ integer_to_list(2235)
+ tuple_to_list(Tuple)

+ Do what you cannot do (or is difficult to do) in Erlang
+ MOstly written in C for fast execution
+ BIFs are by conversion regarded as being in the erlang module.

### Recursion: self-describing code

```erlang
sum([]) -> 0;
sum([H|T]) -> H + sum(T).
```

+ You can read the programs as an executable description
+ "The sum of an empty list is 0."
+ "The sum of a non-empty list is the head of the list added to the sum of the tail."


### Data Types: lists


```erlang
List = [Element + List] or []
```

+ A recursive list definition consistes of a head and a tail
+ Every list is either empty
+ ... or has a head which is an element 
+ and a tail which is a list

### Recursion: traversing lists:

```erlang
printAll([]) -> 
    io:format("~n", []);
printAll([X|Xs]) -> 
    io:format("~p ", [X]),
    printAll(Xs).
```

+ Here we're traversing the list imperatively:
+ "If there are no more elements to process, stop"
+ "If there are further elements, process the head, and then call the function recursively on the tail."

```erlang
printAll(Ys) -> 
    case Ys of
        [] -> 
            io:format("~n", []);
        [X|Xs] ->
            io:format("~p ", [X]),
            printAll(Xs)
    end.
```

+ Same function again:
+ shows the loop clearly.
+ the call to printAll(xs) is like a jump back to the top of the loop
+ Thi is a tail recursive function: The only recursive calls come at the end of the bodies of the clauses

### Recursion: more patterns


```erlang
double([H|T]) -> [2*H|double(T)];
double([]) -> [].

member(H, [H|_]) -> true;
member(X, [_|T]) -> member(X, T);
member(_, []) -> false.

even([H|T]) when H rem 2 == 0 -> [H|even(T)];
even([_|T]) -> even(T);
even([]) -> [].

```

+ double/1 maps elements in a list and returns a new list
+ member/2 is a predicate looking for an element in a list
+ even/1 filters a list of integers and returns the subset of even numbers
+ The function member/2 is the only one which is tail recursive

### Recursion: traversing lists


```erlang
average(X) -> sum(X) / len(X).
sum([H|T]) -> H + sum(T);
sum([]) -> 0.

len([_|T]) -> 1 + len(T);
len([]) -> 0.
```

+ Note the pattern of recursion is the same in both cases
+ Taking a list and evaluating an element is a very common pattern

### Recursion: accumulators

```erlang
average(X) -> average(X, 0,0).
average([H|T], Length, Sum) 0> aveage(T, Length+1, Sum + H);
average([], Length, Sum) -> Sum/Length.

```

+ Only taverses the list once.
+ Executes in constant space(tail recursive)
+ Length and Sum play the role of accuumulators
+ average([]) is not defined
+ Evaluating average([]) would cause a run-time error

## Concurrent Programming


```erlang
spawn(Mod,Func,Args ).
```

+ Before 
  + Code executed by Process 1
  + Process Identifier is Pid1
  + Pid2 = spawn(Mod, Func, Args)
+ After
  + A new process with Pid2 is created
  + Pid2 is only known to Pid1
  + Pid2 runs Mod Func(Args)
  + Mod Func/Arity must be exported
+ Convention: we identify processes by their process ids (pids)


### Creating Processes

```erlang
spawn(Mod, Func, Args).
```

+ The BIF spawn never fails
+ A process terminates
  + abnormally when run-time errors occur
  + normally when there is no more code to execute

```erlang
io:format("Hello~n").
spawn(io, format, ["Hello~n"]).
```

### Message Passing


+ Messages are sendt using the Pid ! Msg expression
  + Msg is from any valid Erlang data type
+ Sending a message will never fail
+ message sent to non-existing processes are thrown away
+ Received messages are stored in the process's mailbox

```erlang
Pid2 ! {self(), foo }

flush(). % get shell process's mailbox
```

```erlang
receive
    {reset, Board} -> reset(Board);
    _Other -> {error, unknown_msg}
end
```

> message passing is asynchronous

```erlang
receive X -> X end.
```

### Receiving Messages

+ Messages can be matched and selectively retrived
+ Messages are received when a message matches a clause
+ Mailboxes are scanned sequentially
+ If Pid is bound before receivng the message, then only data tagged with that pid can be patten matched
+ The variable Digit is bound when receiving the message

```erlang
receive {Pid, {digit, Digit}} -> 
    ---
end.
```

```erlang
receive 
    foo -> true
end.
receive 
    bar -> true
end.
```

+ The message foo is received, followed by the message bar
+ This is irrespective of the order in which they were sent or stored in the mailbox

### Receiving Messages: non-selective

```erlang
receive msg -> true
end.
```

+ The first message to arrive at the process Pid3 will be processed
+ The variable Msg in the process Pid3 will be bound to one of the atomes foo or bar depending on which arrives first

```erlang
Pid = spawn(echo, echo, []).
Pid ! {self(), hello}.
receive X -> X end.

Pid2 = spawn(echo, echo, []).
Pid2 ! {self(), hello}.
Pid2 ! {self(), world}.
exit(Pid2, kill).

```