```erlang
packet_to_term(Bin) ->
    <<Len:4/unit:8, TermBin:Len/binary, _Rest/binary>> = Bin,
    binary_to_term(TermBin).
```

This exercise demonstrates the important point that we can match a Size, then use the match later in the pattern (Ahem!  What happened to maps in this regard?)

In the shell:
```erlang
186> c(bin).                                                     
{ok,bin}

187> f().
ok

188> Packet = bin:term_to_packet([1, 2, 3]).                     
<<0,0,0,7,131,107,0,3,1,2,3>>

189> PacketWithExtra = <<Packet/binary, <<97, 98, 99>>/binary >>.
<<0,0,0,7,131,107,0,3,1,2,3,97,98,99>>

190> bin:packet_to_term(PacketWithExtra).                        
[1,2,3]

```
