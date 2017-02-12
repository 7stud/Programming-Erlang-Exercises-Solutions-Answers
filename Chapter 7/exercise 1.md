`reverse(Bin)`:

You can pretty much use a binary like a list, but instead of using cons, `|`, to add elements to a binary, you use a comma: `<<ToAdd, SomeBinary/binary>>`.  In pattern matching, instead of using:

     [H|T]
    
 for binaries you use:
 
     <<X, Rest/binary>>
     
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
    
The default Type for a segment in a binary is `integer`, so the example above is equivalent to:
    
reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->      %The default Size of the integer Type is 8 bits, and
    reverse(Rest, <<X/integer, Acc/binary>>);    %when matching, if a binary Type is the last segment
reverse(<<>>, Acc) ->                            %its Size can be omitted, and its default Size will
    Acc.                                         %be the rest of the binary that you are matching against.
```

Or, if you wanted to be explicit about the Size *and* the Type:

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






