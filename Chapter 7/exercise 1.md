`reverse(Bin)`.  You can pretty much use a binary like a list, but instead of using cons(|) to add elements to a binary, you just use a comma: `<<X/integer, Y/binary>>`.

```erlang
-module(bin).
-compile(export_all).

reverse(Bin) ->
    reverse(Bin, <<>>).
reverse(<<X/integer, Rest/binary>>, Acc) ->
    reverse(Rest, <<X/integer, Acc/binary>>);
reverse(<<>>, Acc) ->
    Acc.
```
