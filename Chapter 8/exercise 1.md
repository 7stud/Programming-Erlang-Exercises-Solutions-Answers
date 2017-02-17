```A: A lot!```  

I'm too lazy to count the functions exported by the `dict` module (What's a dict??):

```erlang
-module(my).
-compile(export_all).

info(ModName) ->
    Exports = ModName:module_info(exports),
    length(Exports).
```
