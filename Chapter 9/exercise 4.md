#### *Opaque types:*

```erlang

-module(a).
-export([people/0, get_names/1]).
-export_type([person/0]).

-opaque person()      :: {person, first_name()}.
-type first_name()    :: string().

-spec people() -> People when
      People :: [person()].

people() ->
    [{person, "Joe"}, {person, "Cindy"}].

-spec get_names(People) -> Names when
      People     :: [person()],
      Names      :: [string()].

get_names(Ps) ->
    get_names(Ps, []).

get_names([ {person, Name} | Ps ], Names) ->
    get_names(Ps, [Name|Names]);
get_names([], Names) ->
    lists:reverse(Names).
```

```erlang
-module(b).
-export([do/0]).

-spec do() -> Name when
      Name :: string().

do() ->
    [{person, Name} | _People] = a:people(),
    Name.
```

#### *dialyzer:*

```
~/erlang_programs$ dialyzer a.erl b.erl
-----Now, do some Erlang for great Good!------

  Checking whether the PLT /Users/7stud/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
b.erl:7: Function do/0 has no local return
b.erl:8: The attempt to match a term of type a:person() against the 
pattern {'person', Name} breaks the opaqueness of the term
 done in 0m0.50s
done (warnings were emitted)
```
