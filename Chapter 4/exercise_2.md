I used the following functions from the `erlang` module:

```
size/1        Gets the size of a tuple.
element/2     Gets the element of a tuple at the specified index.
```
my.erl:
```erlang
-module(my).
-compile(export_all).   %You use this instead of -export([f/1, g/2, ...]).

tuple_to_list({}) -> [];
tuple_to_list(T) -> tuple_to_list_acc(size(T), T, []).   %Will acces the elements of the tuple starting at the largest 
tuple_to_list_acc(0, _, Acc) -> Acc;                     %index and work down to the smallest index.
tuple_to_list_acc(CurrIndex, T, Acc) ->
   NewAcc = [element(CurrIndex, T) | Acc],
   tuple_to_list_acc(CurrIndex-1, T, NewAcc).
```

In the shell:

```
80> c(my).
{ok,my}

81> my:tuple_to_list({1, 2, 3}).
[1,2,3]

82> my:tuple_to_list({}).       
[]

83> my:tuple_to_list({1, true, "hello"}).
[1,true,"hello"]

84> my:tuple_to_list({1, true, {2.3, "hello"}}).
[1,true,{2.3,"hello"}]
```
