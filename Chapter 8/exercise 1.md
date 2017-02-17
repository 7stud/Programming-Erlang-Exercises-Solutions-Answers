```A: A lot!```  

I'm too lazy to count the functions exported by the `dict` module:

```erlang
-module(my).
-compile(export_all).

export_count(ModName) ->
    length(ModName:module_info(exports) ).

```

What's a dict?  Apparently, maps are supposed to replace dicts, with the benefit of being able to use native syntax instead of function calls, e.g. `M2 = M1#{key := 10}` v. `D2 = dict:store(key, 10, D1)`.
