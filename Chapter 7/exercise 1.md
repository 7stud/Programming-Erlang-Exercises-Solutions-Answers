`reverse(Bin)`:

You can pretty much use a binary like a list, but instead of using cons, `|`, to add elements to a binary, you just use a comma: `<<ToAdd/integer, SomeBinary/binary>>`.

```erlang
-module(bin).
-compile(export_all).

reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->      %The default Size of the integer type is 1 byte, and
    reverse(Rest, <<X/integer, Acc/binary>>);    %the default Size of the binary type is the Size of the specified binary.
reverse(<<>>, Acc) ->
    Acc.
```
