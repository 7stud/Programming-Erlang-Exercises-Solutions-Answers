Joe Armstrong's **ezwebrame** code no longer works.  I asked for help on the erlang-questions forum with some of the errors I was getting, and Joe himself answered and essentially said, "It no longer works.  Tough luck."  Hmmm...I thought the whole point of posting the code on github was to keep it up to date.  Oh, well.

I decided to try and use [gun](https://github.com/ninenines/gun) for the http client, which has websocket support and is maintained by the same person who maintains cowboy, to try to interact with a cowboy server.

To setup cowboy, I followed the cowboy User Guide's [Getting Started](https://ninenines.eu/docs/en/cowboy/2.0/guide/getting_started/) section. Once that was setup correctly, I was at the following prompt:

`(hello_erlang@127.0.0.1)1>`

To setup gun, I opened up another terminal window, and the [gun docs](https://github.com/ninenines/gun/blob/master/doc/src/guide/start.asciidoc) say you can use something called `erlang.mk` and add gun as a dependency to your application.  According to the [Erlang.mk docs](https://erlang.mk/guide/getting_started.html#_getting_started_with_otp_releases), if you create something called _a release_, then you can use the command `make run` (or `gmake run`) to compile and execute your code. After I read the Erlang.mk docs, it wasn't clear to me how to run _an application_ or a _library_, so I opted to create _a release_.  First, I created a directory for my release, then I downloaded erlang.mk:

```
~/erlang_programs$ mkdir my_gun && cd $_

~/erlang_programs/my_gun$ curl -O "https:/erlang.mk/erlang.mk"   
```

Then I used an erlang.mk [command](https://erlang.mk/guide/getting_started.html#_getting_started_with_otp_releases) to create a release:

```
~/erlang_programs/my_gun$ gmake -f erlang.mk bootstrap-lib bootstrap-rel
git clone https://github.com/ninenines/erlang.mk .erlang.mk.build
Cloning into '.erlang.mk.build'...
remote: Counting objects: 7116, done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 7116 (delta 2), reused 6 (delta 1), pack-reused 7105
Receiving objects: 100% (7116/7116), 3.29 MiB | 2.02 MiB/s, done.
Resolving deltas: 100% (4504/4504), done.
if [ -f build.config ]; then cp build.config .erlang.mk.build; fi
cd .erlang.mk.build && gmake
gmake[1]: Entering directory '/Users/7stud/erlang_programs/my_gun/.erlang.mk.build'
export LC_COLLATE=C; \
awk 'FNR==1 && NR!=1{print ""}1' core/core.mk index/*.mk core/index.mk core/deps.mk plugins/protobuffs.mk core/erlc.mk core/docs.mk core/rel.mk core/test.mk core/compat.mk plugins/asciidoc.mk plugins/bootstrap.mk plugins/c_src.mk plugins/ci.mk plugins/ct.mk plugins/dialyzer.mk plugins/edoc.mk plugins/erlydtl.mk plugins/escript.mk plugins/eunit.mk plugins/proper.mk plugins/relx.mk plugins/shell.mk plugins/syntastic.mk plugins/triq.mk plugins/xref.mk plugins/cover.mk plugins/sfx.mk core/plugins.mk core/deps-tools.mk \
	| sed 's/^ERLANG_MK_VERSION =.*/ERLANG_MK_VERSION = 2017.07.06-1-gff27159/' \
	| sed 's:^ERLANG_MK_WITHOUT =.*:ERLANG_MK_WITHOUT = :' > erlang.mk
gmake[1]: Leaving directory '/Users/7stud/erlang_programs/my_gun/.erlang.mk.build'
cp .erlang.mk.build/erlang.mk ./erlang.mk
rm -rf .erlang.mk.build

~/erlang_programs/my_gun$ ls
Makefile	erlang.mk	rel		relx.config	src
```
As you can see, the erlang.mk command that I used created some files and directories.  Next, I edited the Makefile to add gun as a dependency:
```make
PROJECT = my_gun
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = gun

include erlang.mk
```








