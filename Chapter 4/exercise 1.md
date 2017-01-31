geometry.erl:

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
    Side1 + Side2 + math:sqrt((Side1*Side1) + (Side2*Side2))
```

In the shell:

```erlang
52> c(geometry).
{ok,geometry}

53> geometry:perimeter({square, 2}).
8

54> geometry:perimeter({rectangle, 2, 4}).
12

55> geometry:perimeter({circle, 4}).      
25.132741228718345

56> geometry:perimeter({right_triangle, 3, 4}).
12.0

58> geometry:area({circle, 10}).
314.2

59> geometry:area({right_triangle, 3, 4}). 
6.0


```
