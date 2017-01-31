```erlang

-module(geometry).
-export([area/1, test/0, perimeter/1]).  %Add perimeter() to list of exports.

test() ->
    4 = area({square, 2}),
    10 = area({rectangle, 5, 2}),
    tests_worked.

area({square, Side}) ->
    Side * Side;
area({rectangle, Height, Width}) ->
    Height * Width;
area({circle, Radius}) ->
    3.142 * Radius * Radius;
area({right_triangle, Side1, Side2}) ->
    0.5 * Side1 * Side2.

perimeter({square, Side}) ->
    4 * Side;
perimeter({rectangle, Height, Width}) ->
    (2 * Height) + (2 * Width);
perimeter({circle, Radius}) ->
    2 * math:pi() * Radius;
perimeter({right_triangle, Side1, Side2}) ->
    Side1 + Side2 + math:sqrt((Side1*Side1) + (Side2*Side2)).



```
