```erlang
packet_to_term(Bin) ->
    <<Size:4, TermBin:Size/binary, _Rest/binary>> = Bin,
    binary_to_term(TermBin).
```

This exercise demonstrates the important point that we can match a Size then use the match later in the pattern (Ahem!  What happened to maps in this regard?)

In the shell:
```erlang
77> c(bin).
{ok,bin}

78> f().                                                
ok

79> Packet = bin:term_to_packet([1, 2, 3]).             
<<120,54,176,0,48,16,32,3:4>>

80> Bin = << Packet/bitstring, <<97, 98, 99>>/binary >>.  %Because Packet does not contain a whole
<<120,54,176,0,48,16,32,54,22,38,3:4>>                    %number of bytes, its type is bitstring.

81> bin:packet_to_term(Bin).                            
[1,2,3]
```
