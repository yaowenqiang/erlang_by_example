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




