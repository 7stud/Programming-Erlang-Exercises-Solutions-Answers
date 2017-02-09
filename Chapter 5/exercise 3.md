I know ruby pretty well, and once again, just like the python datetime exercise in Chapter 4, this is a vast exercise. A Hash in ruby is equivalent to a map in erlang.  One thing you have to know about ruby is that a *block* in ruby is similar to an anonymous function, which is equivalent to a fun in erlang.  So ruby methods that take a block are methods that accept an anonymous function as an argument.  Here is how to interpret the ruby docs:

    any? [{ |(key, value)| block }] → true or false 
    
The bit inside the `[ ]` means it's optional when you call the method named `any?`, but let's pretend the optional part is required, giving you
 
     any? { |(key, value)| block } → true or false 
     
The stuff inside the `{ }` is known as a block, which is just an anonymous function that will be passed to the method named `any?`.  Yes, in ruby a method name can contain the `?` character.  The `?` character signals that the method returns true or false.  And no, ruby does not use `{}` in place of `()` for method calls.  If `any?` took a non-function argument you would call `any?` like this:

    any?(arg1) {|(key, val)| key+val == 10}
    
Rather, ruby has a special syntax for sending an anonymous function to a method: you specify a block, denoted with `{ }`, after the method call.
    
The part between the pipes:

    |(key, value)|
    
is the parameter list for the anonymouse function. And `block`, somewhat confusingly, is just a standin for some code. The code should return true or false for each key/value pair in the Hash.  The return value of `any?` is signified by:

    → true or false 

The `any?` method traverses a Hash looking for an element for which block(Key, Val) returns true, and if found `any?`halts and immediately returns true.  If the block never returns true, then `any?` returns false.

Here is my implementation of Ruby's `Hash#any?` method, which is very similar to the `map_search_pred()` function in Exercise 2:

```erlang
-module(my).
-compile(export_all).

any(Map, Pred) -> any(maps:keys(Map), Map, Pred).

any([Key|Keys], Map, Pred) ->
    Val = maps:get(Key, Map),
    
    case Pred(Key, Val) of
        true -> true;
        false -> any(Keys, Map, Pred)
    end;
any([], _, _) ->   %If the entire Keys list was traversed and Pred never returned true, return false.
    false.
```

In the shell:

```erlang
85> f().

86> Map = #{                            
86> 1 => 10,
86> 2 => 20,
86> 3 => 3 
86> }.
#{1 => 10,2 => 20,3 => 3}

87> c(my).

88> Pred1 = fun(X, Y) -> X =:= Y end.   
#Fun<erl_eval.12.52032458>

89> Pred2 = fun(X, Y) -> X+Y =:= 22 end.
#Fun<erl_eval.12.52032458>

90> Pred3 = fun(X, Y) -> X*Y =:= 0 end. 
#Fun<erl_eval.12.52032458>

93> my:any(Map, Pred1).                 
true

94> my:any(Map, Pred2).
true

95> my:any(Map, Pred3).
false
```



