The functions `maps:to_json(Map)`, etc. don't exist, so I installed `rebar3` and I created an erlang project that specifies `jsx` as a dependency.  `jsx` is a third party json handling library for erlang.  To get setup with rebar3, see [here](http://stackoverflow.com/questions/34278982/what-is-the-easiest-way-for-beginners-to-install-a-module).

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

Figuring out how to do a sanity check on nested json seemed too difficult to me, so I thought I would start out with an easier configuration file:

```javascript
{
  "src_path": "a/b/c",
  "connection_name": "google",
  "log_level": "warn",
  "in_memory":  true,
  "port": 2707,
}
```

My approach was to write a function that accepts the ConfigMap, as well as a SanityMap, where the SanityMap contains keys that match some or all of the keys in the ConfigMap, and the values in the SanityMap are (white) lists of accepted values for that key.

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





