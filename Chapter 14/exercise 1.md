In Terminal window #1:
```
~$ erl -sname gandalf -setcookie abc
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

(gandalf@7studsMBP)1> 
```

In Terminal window #2:
```
$ erl -sname bilbo -setcookie abc
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

(bilbo@7studsMBP)1> nodes().
[]

(bilbo@7studsMBP)2> net_adm:ping('gandalf@7studsMBP').   %Activate the gandalf Node.
pong

(bilbo@7studsMBP)3> nl(ml).   %Load the ml (mylib) module on the gandalf Node.
abcast

(bilbo@7studsMBP)4> ml:start('gandalf@7studsMBP').
Enter your name: 7stud
Hello, 7stud!
ok
(bilbo@7studsMBP)5> 
```

The code:
```erlang
-module(ml).
-compile(export_all).

get_greeting() ->
    timer:sleep(1000 * rand:uniform(5)),  %Mimic doing some work, like accessing a website to get interesting greetings.
    Greetings = ["Hello", "Good day", "Fine morning"],
    lists:nth(
      rand:uniform(length(Greetings)),
      Greetings).

start(Node) ->
    Key = rpc:async_call(Node,
                         ml, get_greeting, [] ),

    Input = io:get_line("Enter your name: "),  %While the user is entering their name, the other node is doing some work.
    Name = string:strip(Input, right, $\n),
    io:format("~s, ~s!~n", [rpc:yield(Key), Name]). %yield() blocks and watis for the return  
                                                    %value if Key process hasn't returned yet.
```    
