```A: A lot!```  

I'm too lazy to count the functions exported by the `dict` module:

```erlang
-module(my).
-compile(export_all).

info(ModName) ->
    Exports = ModName:module_info(exports),
    length(Exports).
```

What's a dict?  Apparently, maps are supposed to replace dicts, with the benefit of being able to use native syntax instead of function calls, e.g. `M1#{key := 10}` v. `dict:store(key, 10, M1)`.
