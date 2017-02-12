`reverse(Bin)`:

You can pretty much use a binary like a list, but instead of using cons, `|`, to add elements to a binary, you just use a comma: `<<ToAdd/integer, SomeBinary/binary>>`; and in pattern matching instead of using `[H|T]`, you use `<<X/integer, Rest/binary>>`:

```erlang
-module(bin).
-compile(export_all).

reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->      %The default Size of the integer Type is 8 bits, and
    reverse(Rest, <<X/integer, Acc/binary>>);    %when matching, if a binary Type is the last segment
reverse(<<>>, Acc) ->                            %its Size can be omitted, and its default Size will
    Acc.                                         %be the rest of the binary that you are matching against.
```

Because `integer` is the default Type of a segment in a binary, you could write the above example like this:

```erlang
reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X, Rest/binary>>, Acc) ->
    reverse(Rest, <<X, Acc/binary>>);
reverse(<<>>, Acc) ->
    Acc.
```

Or, if you wanted to be explicit about the Size and the Type:





