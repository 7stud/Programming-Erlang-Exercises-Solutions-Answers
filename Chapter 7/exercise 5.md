```erlang

reverse_bits(Bin) ->
    reverse_bits(Bin, <<>>).
reverse_bits(<<X:1, Rest/bitstring>>, Acc) ->
    reverse_bits(Rest, << X:1, Acc/bitstring >>);
reverse_bits(<<>>, Acc) ->
    Acc.
```

In the shell:
```erlang
100> c(bin).                                     
{ok,bin}

101> f().
ok

102> X = 2#11010101.
213

103> Bin = <<X>>.   
<<"Õ">>

104> Result = bin:reverse_bits(Bin).
<<"«">>

105> <<Y>> = Result.  %Extract the integer from the binary.
<<"«">>

106> io:format("Y:~8.2.0B~nX:~8.2.0B~n", [Y, X]).
Y:10101011
X:11010101
ok
```
