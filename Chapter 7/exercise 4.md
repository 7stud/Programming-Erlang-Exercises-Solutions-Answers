```erlang
tests() ->
    %term_to_packet():
    <<3:4/unit:8,  131,97,10>> = term_to_packet(10),
    <<9:4/unit:8,  131,100,0,5,104,101,108,108,111>>  = term_to_packet(hello),
    <<23:4/unit:8, 131,104,3,100,0,2,104,105,70,64,9,153,153,153,
      153,153,154,107,0,3,1,2,3>> =  term_to_packet( {hi, 3.2, [1, 2, 3]} ),

    %packet_to_term():
    10 = packet_to_term(<<0,0,0,3, 131,97,10>>),
    hello = packet_to_term(<<0,0,0,9, 131,100,0,5,104,101,108,108,111>>),
    {hi, 3.2, [1, 2, 3]} = packet_to_term(<<0,0,0,23,131,104,3,100,0,2,
            104,105,70,64,9,153,153,153,153,153,154,107,0,3,1,2,3>>),

    %round trip:
    Term1 = [3.12, hello, {abc, 1}],
    Term1 = packet_to_term(term_to_packet([3.12, hello, {abc, 1}])),
    Term2 = <<97, 98, 99>>,
    Term2 = packet_to_term(term_to_packet(<<97, 98, 99>>)),

    tests_passed.
```
