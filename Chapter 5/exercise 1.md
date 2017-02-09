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



