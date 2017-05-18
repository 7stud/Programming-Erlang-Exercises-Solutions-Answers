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

(You are not supposed to use extensions for shell script file names, e.g. run.sh, but using a .sh/.bash extension will allow my editor's syntax highlighting to take effect.)

Then I made it executable:
```
$ chmod u+x run
```
Now, I can run my elrang program from the command line like this:
```
$ ./run
```
When I need to run my program again, I kill the erlang shell with `Ctrl+CC`, then I can just hit the up arrow on my keyboard, which will scroll to the previous command, then I hit Return.  No more typing in the shell!  

Potential problems when using a shell script:

1. Sometimes after running my shell script on a particular program, the shell is unresponsive, so I can't check `i()` or run `observer:start()`.  If the shell is unresponsive after I run my shell script, and I need the shell to be responsive, then I create a function called `test_init()`, which just spawns my `test()` function; then I substitute the name `test_init` in place of `test` in my shell script.  

2. The program output will interleave with the output that the shell displays on startup.  To keep things tidy, I begin my `test()` functions with a short `timer:sleep(500)`.  That way the output from the erlang shell initialization has time to print before my erlang program begins outputting text.

3. Sometimes the last line of output won't be the same as in the erlang sheel, so I have to resort to using the erlang shell again to confirm that everything is working correctly.  A shell script runs in a different process than the erlang shell, and the script process is not in a read-evaluate loop like the erlang shell, so the script process won't automaticaly output the result of the expression on the last line of your program.  Of course, you can just call `io:format()` to print out the value of that last expression.

Despite those three issues, I still find that using a shell script to run an erlang program is a _massive_ time saver, and being able to rerun your program quickly by hitting the up arrow, then hitting Return will keep you from going insane during long debugging sessions.

