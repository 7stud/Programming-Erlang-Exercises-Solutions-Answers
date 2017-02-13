`reverse(Bin)`:

Constructing a binary is much like constructing a list, but instead of using cons, `|`, to add element to a binary you use a comma: `<< ItemToAdd, SomeBinary/binary >>`.  And when pattern matching binaries, the equivalent of:

     [H|T]
    
is: 
 
     <<X, Rest/binary>>

Here's an example in the shell:
```erlang
15> f().                                     
ok

16> <<X, Rest/binary>> = <<97, 98, 99, 100>>.
<<"abcd">>

17> X.
97

18> Rest.
<<"bcd">>
```

Or, you could do this:
```erlang
20> f().
ok

21> <<X:1/binary, Rest/binary>> = <<97, 98, 99, 100>>.
<<"abcd">>

22> X.
<<"a">>

23> Rest.
<<"bcd">>
```
Note that the Size is specified in bits, but the total size of a segment is actually `Size * units`, and for the binary Type the default for units is 8, so the total size of X is `1*8 = 8 bits`.  In case you were wondering, the default units for the other Type's is just 1.

Here's the function:

```erlang
-module(bin).
-compile(export_all).

reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X, Rest/binary>>, Acc) ->
    reverse(Rest, <<X, Acc/binary>>);
reverse(<<>>, Acc) ->
    Acc.
```

The default Type for a segment in a binary is `integer`, so the example above is equivalent to:
```erlang
reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->      %The default Size of the integer Type is 8 bits, and
    reverse(Rest, <<X/integer, Acc/binary>>);    %when matching, if a binary Type is the last segment
reverse(<<>>, Acc) ->                            %its Size can be omitted, and its default Size will
    Acc.                                         %be the rest of the binary that you are matching against.
```

Or, if you want to be explicit about the Size *and* the Type:

```erlang
reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X:8/integer, Rest/binary>>, Acc) ->
    reverse(Rest, <<X:8/integer, Acc/binary>>);
reverse(<<>>, Acc) ->
    Acc.
```

In the shell:
```erlang
10> c(bin).
{ok,bin}

11> bin:reverse(<<1, 2, 3, 4>>).
<<4,3,2,1>>

12> bin:reverse(<<>>).
<<>>

13> bin:reverse(<<97, 98, 99>>).
<<"cba">>
```






