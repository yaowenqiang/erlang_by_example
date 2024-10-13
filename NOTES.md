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
