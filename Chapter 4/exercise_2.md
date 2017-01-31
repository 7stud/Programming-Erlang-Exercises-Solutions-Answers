I used the following `erlang` module functions:

```
erlang:size/1        Gets the size of a tuple.  
erlang:element/2     Gets the element of a tuple at the specified index.
```
my.erl
```erlang
-module(my).
-compile(export_all).

tuple_to_list({}) -> [];
tuple_to_list(T) -> tuple_to_list_acc(size(T), T, []).   %Will acces the elements of the tuple starting at the largest 
                                                         %index and working down to the smallest index.

tuple_to_list_acc(0, _, Acc) -> Acc;
tuple_to_list_acc(CurrIndex, T, Acc) ->
   NewAcc = [element(CurrIndex, T) | Acc],
   tuple_to_list_acc(CurrIndex-1, T, NewAcc).
```


