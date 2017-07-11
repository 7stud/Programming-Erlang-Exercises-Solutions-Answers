Joe Armstrong's **ezwebrame** code no longer works.  I asked for help on the erlang-questions forum with some of the errors I was getting, and Joe himself answered and essentially said, "It no longer works.  Tough luck."  Hmmm...I thought the whole point of posting the code on github was to keep it up to date.  Oh, well.

I decided to try and use [gun](https://github.com/ninenines/gun) for the http client, which has websocket support and is maintained by the same person who maintains cowboy, in order to interact with a cowboy server.

To setup cowboy, I followed the cowboy User Guide's [Getting Started](https://ninenines.eu/docs/en/cowboy/2.0/guide/getting_started/) section.

To setup gun, the [gun docs](https://github.com/ninenines/gun/blob/master/doc/src/guide/start.asciidoc) say you can use something called erlang.mk and add gun as a dependency.  According to the [Erlang.mk docs](https://erlang.mk/guide/getting_started.html#_getting_started_with_otp_releases), if you create something called _a release_, then you can use the command `make run` (or `gmake run`) to compile and execute your code. After reading the Erlang.mk docs it wasn't clear to me how to run _an application_ or a _library_, so I opted to create _a release_:



```







