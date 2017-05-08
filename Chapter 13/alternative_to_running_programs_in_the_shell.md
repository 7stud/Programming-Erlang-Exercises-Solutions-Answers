I am sick of typing:

```
$ erl

1> c(e1).

2> e1:test().

```
over and over again.  I shortened my module names to two letters, which makes things easier, but it's still a pain.  So I created a shell script in the same directory called `run`:

```
#!/usr/bin/env bash

erlc -W e1.erl
erl -s e1 test
```

Then I made it executable:
```
$ chmod u+x run
```
Now, I can run my elrang program from the command line like this:
```
$ ./run
```
When I need to run my program again, I kill the erlang shell with `Ctrl+CC`, then I can just hit the up arrow on my keyboard, which will scroll to the previous command, and hit Return.  No more typing in the shell!  

Note that sometimes after running my shell script, the shell will be unresponsive, so I can't check `i()` or run `observer:start()`.  If the shell is unresponsive after I run my shell script, and I need the shell to be responsive, then I create a function called `test_init()`, which just spawns my `test()` function; then I substitute the name `test_init` in place of `test` in my shell script.  Keep that trick in mind if you use a shell script.
