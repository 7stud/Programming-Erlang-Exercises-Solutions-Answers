The json functions mentions on p.84 don't exist, e.g. `maps:to_json(Map)`, so I installed `rebar3` and I created an erlang project that specifies `jsx` as a dependency.  `jsx` is a third party json handling library for erlang.  To get setup with rebar3 and jsx see [here](http://stackoverflow.com/questions/34278982/what-is-the-easiest-way-for-beginners-to-install-a-module).

Oh boy, file I/O without any guidance from the book!

Reading configuration files:

```erlang
-module(my).
-compile(export_all).

read_config(Fname) ->
    case file:read_file(Fname) of
        {ok, Binary}    ->  
            convert_to_map(Binary);
        {error, Reason} -> 
            Reason
    end.

convert_to_map(Binary) ->
    jsx:decode(Binary, [return_maps]).
```

my_config.config:

```javascript
{
  "log_level": "warn",
  "in_memory":  true,
  "port": 2707,
  "runner_dirs": ["/dir1", "/dir2"],
  "mappings": {"a": [1, 2, 3], "b": {"name": "Mike", "digits": 123} }
}
```

In the shell:
```erlang
29> c(my).
{ok,my}

30> ConfigMap = my:read_config("my_config.config").
#{<<"in_memory">> => true,
  <<"log_level">> => <<"warn">>,
  <<"mappings">> => #{<<"a">> => [1,2,3],
    <<"b">> => #{<<"digits">> => 123,<<"name">> => <<"Mike">>}},
  <<"port">> => 2707,
  <<"runner_dirs">> => [<<"/dir1">>,<<"/dir2">>]}
```

Oh boy, dealing with binaries without them having been introduced yet in the book.  Well, they look like strings with `<<` and `>>` around them, e.g. `<<"hello">>`.

Figuring out how to do a sanity check on deeply nested json seemed too difficult to me, so I thought I would start out with an easier configuration file:

```javascript
{
  "log_level": "warn",
  "in_memory":  true,
  "port": 2707,
}
```

My approach was to write a function that accepts the ConfigMap, as well as a SanityMap, where the SanityMap contains keys that match some or all of the keys in the ConfigMap, and the values in the SanityMap are (white) lists of accepted values for that key.  For instance, here is the SanityMap I used:

```erlang
48> SanityMap = #{
48> <<"log_level">> => [<<"warn">>, <<"debug">>], 
48> <<"in_memory">> => [true, false], 
48> <<"port">> => lists:seq(2700, 2800)
48> }.

#{<<"in_memory">> => [true,false],
  <<"log_level">> => [<<"warn">>,<<"debug">>],
  <<"port">> => [2700,2701,2702,2703,2704,2705,2706,2707,2708,2709,2710,
   2711,2712,2713,2714,2715,2716,2717,2718,2719,2720,2721,2722,
   2723,2724,2725|...]}
```

The return value of my `sanity_check()` function is a map with the keys in the ConfigMap and values of true or false to indicate whether they passed the sanity check:

```erlang
sanity_check(SanityMap, DataMap) ->
    sanity_check_acc(
      maps:keys(DataMap), 
      DataMap, 
      SanityMap, 
      #{}
    ).

sanity_check_acc([Key|Keys], DataMap, SanityMap, AccMap) ->
    DMVal = maps:get(Key, DataMap),
    case SanityMap of  %Mimic if-else with pattern matching 
        #{Key := WhiteList} ->  %Pattern matching maps is broken in erlang 17.5,
            sanity_check_acc(    %so I used erlang 19.2, which is only slightly better.
              Keys, 
              DataMap,
              SanityMap, 
              AccMap#{Key => lists:member(DMVal, WhiteList)}
            );
        _ -> sanity_check_acc(
               Keys,
               DataMap,
               SanityMap,
               AccMap
             )
    end;
sanity_check_acc([], _, _, AccMap) ->
    AccMap.
```

In the shell:

```erlang
37> c(my).
{ok,my}

38> f(ConfigMap).
ok

39> ConfigMap = my:read_config("my_config.config").                                                                  #{<<"in_memory">> => true,
  <<"log_level">> => <<"warn">>,
  <<"port">> => 2707}
  
40> SanityMap.                                     
#{<<"in_memory">> => [true,false],
  <<"log_level">> => [<<"warn">>,<<"debug">>],
  <<"port">> => [2700,2701,2702,2703,2704,2705,2706,2707,2708,2709,2710,
   2711,2712,2713,2714,2715,2716,2717,2718,2719,2720,2721,2722,
   2723,2724,2725|...]}
   
41> my:sanity_check(SanityMap, ConfigMap).         
#{<<"in_memory">> => true,<<"log_level">> => true,<<"port">> => true}

42> SanityMap2 = SanityMap#{<<"log_level">> => [<<"debug">>, <<"info">>]}.
#{<<"in_memory">> => [true,false],
  <<"log_level">> => [<<"debug">>,<<"info">>],
  <<"port">> => [2700,2701,2702,2703,2704,2705,2706,2707,2708,2709,2710,
   2711,2712,2713,2714,2715,2716,2717,2718,2719,2720,2721,2722,
   2723,2724,2725|...]}
   
43> my:sanity_check(SanityMap2, ConfigMap).                               
#{<<"in_memory">> => true,<<"log_level">> => false,<<"port">> => true}
```




