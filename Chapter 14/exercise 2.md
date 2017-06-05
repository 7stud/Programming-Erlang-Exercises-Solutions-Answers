In a Terminal window on computer #1 (or using an ssh session on computer #2 like I did--see instructions below):
```
$ ssh FirstLast@new-host.home
Password:
Last login: Mon Jun  5 00:51:31 2017 from 7studsmbp.home

~$ erl -name gandalf -setcookie abc
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

(gandalf@new-host.home)1> 
```

In another Terminal window on computer #2:
```
$ erl -name bilbo -setcookie abc
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

(bilbo@7studsMBP.home)1> nodes().
[]

(bilbo@7studsMBP.home)2> net_adm:ping('gandalf@new-host.home').
pong

(bilbo@7studsMBP.home)3> nodes().
['gandalf@new-host.home']

(bilbo@7studsMBP.home)4> c(ml).
{ok,ml}

(bilbo@7studsMBP.home)5> nl(ml).
abcast

(bilbo@7studsMBP.home)6> ml:start('gandalf@new-host.home').
Enter your name: 7stud
Fine morning, 7stud!
ok

(bilbo@7studsMBP.home)7>
```

The code:
```erlang
-module(ml).
-compile(export_all).

get_greeting() ->
    timer:sleep(1000 * rand:uniform(5)),  %Mimic doing some work, like accessing websites to get interesting greetings.
    Greetings = ["Hello", "Good day", "Fine morning"],
    lists:nth(
      rand:uniform(length(Greetings)),
      Greetings).

start(Node) ->
    Key = rpc:async_call(Node,
                         ml, get_greeting, [] ),  %Execution continues immediately on the next line.

    Input = io:get_line("Enter your name: "),  %While the user is entering their name, the other node is doing some work.
    Name = string:strip(Input, right, $\n),
    io:format("~s, ~s!~n", [rpc:yield(Key), Name]). %If Key process hasn't returned yet,
                                                    %yield() blocks and waits for the return value.
                                                    
```    

### ssh setup:

Mac#1 (the Server) iMac running OSX 10.7.5/Erlang 19.2: 
------------------- 
1. System Preferences > Security & Privacy > Firewall:  
--Turn Off Firewall

2. System Preferences > Sharing:   
--Check Remote Login checkbox.   
--Allow access for: Only these Users: Select a user whose password you know.   
--The green light next to “Remote Login: On” should be lit.   
--Underneath the green light it says: To log in to this computer remotely type:   
“ssh FirstLast@new-host.home”.   
You will use whatever it says on that line (without the quotes) for starting an ssh session.  

Mac#2 (the Client) Macbook Pro running OSX 10.10.5/Erlang 19.2: 
--------------------- 
1. Open a Terminal window.  

2. Start an ssh session:   
```~$ ssh FirstLast@new-host.home```

(The first time I opened an ssh session, I was presented with a message that said something like, “blah blah keys couldn’t be verified. Continue (yes/no)?” I typed: yes)  
```
Password: <enter password for the User chosen in step #2 for Mac#1>   
Last login: Mon Dec 1 11:30:22 1900 from MyMB.home

~$ cd erlang_programs/ (this is the directory on Mac#1 that contains kvs.erl and kvs.beam. 
~/erlang_programs$ ls 
kvs.beam	kvs.erl

~/erlang_programs$ erl -name gandalf -setcookie abc   
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]   
Eshell V8.2 (abort with ^G)  

(gandalf@new-host.home)1> kvs:start().   
true   
(gandalf@new-host.home)2>   
```
4. Open another Terminal window.

5. It doesn’t matter what directory you are in: 
```
$ erl -name bilbo -setcookie abc   
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]   
Eshell V8.2 (abort with ^G)  

(bilbo@MyMBP.home)1>  
```
6. 
```
(bilbo@MyMBP.home)1> rpc:call('gandalf@new-host.home', kvs, store, [weather, cold]).   
true
```
Note that the fully qualified host name needs to be atom quoted--otherwise you will get the error: * 1: syntax error before: '.' Also, note that the fully qualified host name that worked for me is the exact same thing appearing in the prompt for the ssh session in the first Terminal window.

7. 
```
(bilbo@MyMBP.home)2> rpc:call('gandalf@new-host.home', kvs, lookup, [weather]).   
{ok,cold}   
(bilbo@MyMBP.home)3>   
```
8. You can end the ssh session like this:
```
(bilbo@MyMBP.home)3> q().   
~/erlang_programs$ exit   
logout   
Connection to new-host.home closed.    
~$  
```
Mac#1 (the Server): 
------------------- 
1. System Preferences > Sharing:   
--Uncheck Remote Login checkbox.

2. System Preferences > Security & Privacy > Firewall:   
--Turn On Firewall
