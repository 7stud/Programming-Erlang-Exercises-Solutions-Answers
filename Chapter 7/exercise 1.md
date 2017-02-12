`reverse(Bin)`:

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
