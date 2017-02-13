```erlang
packet_to_term(Bin) ->
    <<Size:4, TermBin:Size/binary>> = Bin,
    binary_to_term(TermBin).
```
