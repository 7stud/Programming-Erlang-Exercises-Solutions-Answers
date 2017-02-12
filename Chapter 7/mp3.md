I got interested in the example in this chapter about finding the synchronization header in an mp3 file.   I wanted to see if it would work on some random, free mp3 file I downloaded. There is a [treatise](http://mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm) explaining the MPEG header format, and it presents the formula for calculating the frame length.  After borrowing some tricks I discovered in the [full code for the example](https://github.com/everpeace/programming-erlang-code/blob/master/code/mp3_sync.erl), here is what I ended up with:

```erlang
-module(mp3).
-compile(export_all).

sync_pos(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    find_sync(Bin, 1140).

find_sync(Bin, Pos) ->
    case get_header(Bin, Pos) of
        {ok, FrameLen1} ->
            case get_header(Bin, Pos+FrameLen1) of
                {ok, FrameLen2} -> 
                    case get_header(Bin, Pos+FrameLen1+FrameLen2) of
                        {ok, _ } -> {ok, Pos};
                        error -> find_sync(Bin, Pos+1)
                    end;
                error -> 
                    find_sync(Bin, Pos+1)
            end;
        error -> 
            find_sync(Bin, Pos+1)
    end.


get_header(Bin, N) ->
    try
        {_, << Header:4/binary, _/binary >>} = split_binary(Bin, N),
        unpack_header(Header)
    catch
        error:{badmatch, _} -> error(eof);  %The only way you get a bad match
        _:_ -> error                        %is if N is less than four bytes
    end.                                    %from the end of Bin.                              


unpack_header(<< 2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, _Bits:9 >>) ->
    Version = case B of 
                0 -> {2,5};
                1 -> exit(bad_version);
                2 -> 2;
                3 -> 1
              end,
    Layer = case C of
                0 -> exit(bad_layer);
                1 -> 3;
                2 -> 2;
                3 -> 1
            end,
    BitRateIndex = E,
    SampleRateIndex = F,
    Pad = G,

    BitRate = bit_rate(
            BitRateIndex,
            Version,
            Layer
    ),
    SampleRate = sample_rate(
            SampleRateIndex,
            Version
    ),

    {ok, frame_length(Layer, BitRate, SampleRate, Pad) };

unpack_header(_) ->
    error.

frame_length(Layer, BitRate, SampleRate, Pad) ->
    case Layer of 
        1   ->
            (12*(BitRate div SampleRate) + Pad) * 4;

        _   ->
            144*(BitRate div SampleRate) + Pad
    end.

% *******> Throws error for E = 0000 and E = 1111 <*************
bit_rate(2#0000, _, _) -> 
    io:format("***free format BitRateIndex: ~4.2.0B~n", [2#0000]),
    exit(freeFormatBitRateIndex);

bit_rate(2#1111, _, _) -> 
    io:format("***Illegal BitRateIndex: ~4.2.0B~n", [2#1111]),
    exit(badBitRateIndex);

bit_rate(BitRateIndex, 1, 1) ->
    BitRate = element(
        BitRateIndex, 
        {32,64,96,128,160,192,224,256,288,320,352,384,416,448}
    ),
    io:format(
       "bitrate for ~4.2.0B, V~w, L~w: ~w~n", 
        [BitRateIndex, 1, 1, BitRate]
    ),
    BitRate;

bit_rate(BitRateIndex, 1, 2) ->
    BitRate = element(
        BitRateIndex,
        {32,48,56,64,80,96,112,128,160,192,224,256,320,384}
    ),
    io:format(
       "bitrate for ~4.2.0B, V~w, L~w: ~w~n", 
        [BitRateIndex, 1, 2, BitRate]
    ),
    BitRate;

bit_rate(BitRateIndex, 1, 3) ->
    BitRate = element(
        BitRateIndex,
        {32,40,48,56,64,80,96,112,128,160,192,224,256,320}
    ),
        io:format(
       "bitrate for ~4.2.0B, V~w, L~w: ~w~n", 
        [BitRateIndex, 1, 3, BitRate]
    ),
    BitRate;

bit_rate(BitRateIndex, 2, 1) ->
    BitRate = element(
        BitRateIndex,
        {32,48,56,64,80,96,112,128,144,160,176,192,224,256}
    ),
    io:format(
       "bitrate for ~4.2.0B, V~w, L~w: ~w~n", 
        [BitRateIndex, 2, 1, BitRate]
    ),
    BitRate;

bit_rate(BitRateIndex, 2, 2) ->
    BitRate = element(
        BitRateIndex,
        {8,16,24,32,40,48,56,64,80,96,112,128,144,160}
    ),
    io:format(
       "bitrate for ~4.2.0B, V~w, L~w: ~w~n", 
        [BitRateIndex, 2, 2, BitRate]
    ),
    BitRate;

%MPEG Version 2, Layer3 is equivalent to MPEG Version2, Layer2:
bit_rate(BitRateIndex, 2, 3) ->
    bit_rate(BitRateIndex, 2, 2);

%MPEG Version 2.5 is equivalent to MPEG Version 2: 
bit_rate(BitRateIndex, {2,5}, Layer) ->
    bit_rate(BitRateIndex, 2, Layer).


% **** Throws error for F = 2#11 **********
sample_rate(2#11, _) ->
    io:format("***Illegal SampleRateIndex: ~4.2.0B~n", [2#11]),
    exit(badSampleRateIndex);
sample_rate(F, 1) ->
    SampleRate = element( F+1, {44100, 48000, 32000} ),
    io:format(
        "sample rate for 2#~2.2.0B, ~w: ~w~n",
        [F, 1, SampleRate]
    ),
    SampleRate;
sample_rate(F, 2) ->
    SampleRate = element(F+1, {22050, 24000, 16000} ),
    io:format(
        "sample rate for 2#~2.2.0B, ~w: ~w~n",
        [F, 2, SampleRate]
    ),
    SampleRate;
sample_rate(F, {2,5}) ->
    SampleRate = element(F+1, {11025, 12000, 8000} ),
    io:format(
        "sample rate for 2#~2.2.0B, ~w: ~w~n",
        [F, 2.5, SampleRate]
    ),
    SampleRate.  
```
