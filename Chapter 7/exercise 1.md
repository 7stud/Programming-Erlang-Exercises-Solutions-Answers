`reverse(Bin)`:

You can pretty much use a binary like a list, but instead of using cons, `|`, to add elements to a binary, you just use a comma: `<<ToAdd/integer, SomeBinary/binary>>`; and in pattern matching instead of using `[H|T]`, you use `<<X/integer, Rest/binary>>`:

```erlang
-module(bin).
-compile(export_all).

reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->      %The default Size of the integer type is 1 byte, and
    reverse(Rest, <<X/integer, Acc/binary>>);    %when matching, the default Size of the binary type when
reverse(<<>>, Acc) ->                            %it's the last segment in the pattern is the rest of the 
    Acc.                                         %binary that you are matching against.
```
