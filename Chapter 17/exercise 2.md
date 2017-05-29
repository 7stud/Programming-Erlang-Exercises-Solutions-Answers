```erlang
-module(e2).
%%-compile(export_all).
-export([server_init/0, client/3, sum/1, go/0]).

server_init() ->
    {ok, ServerSocket} = gen_tcp:listen(33133, [binary, {active,true}, 
                                               {packet,4},{reuseaddr,true}] ),
    io:format("Server started on port: 33133~n"),

    {ok, ClientSocket} = gen_tcp:accept(ServerSocket),
    gen_tcp:close(ServerSocket),
    server_loop(ClientSocket).

server_loop(ClientSocket) ->
    receive
        {tcp, ClientSocket, WholeMsg} ->
            case binary_to_term(WholeMsg) of
                {Mod, Func, Args} ->
                    Reply = apply(Mod, Func, Args),
                    io:format("Server: result is ~w~n", [Reply]),
                    gen_tcp:send(ClientSocket, term_to_binary(Reply));
                _Other ->
                    ok
            end;
        {tcp_closed, ClientSocket} ->
            io:format("Server: client closed socket.~n")
    end.


client(Mod, Func, Args) ->
    {ok, Socket} = 
        gen_tcp:connect(localhost, 33133, [binary, {active,true},
                                           {packet,4}, {reuseaddr,true}] ),
    Request = term_to_binary({Mod, Func, Args}),
    gen_tcp:send(Socket, Request),
    receive
        {tcp, Socket, WholeReply} ->
            io:format("Client: received ~w~n", [binary_to_term(WholeReply)]),
            gen_tcp:close(Socket);
        {tcp_closed, Socket} ->
            io:format("Client: server closed the socket.~n")
    end.

sum(Args) ->
    sum(Args, 0).

sum([Arg|Args], Sum) ->
    sum(Args, Sum+Arg);
sum([], Sum) ->
    Sum.

go() ->
    timer:sleep(500),  %%Allow time for shell startup
    spawn(?MODULE, server_init, []),
    client(?MODULE, sum, [[1, 2, 3]]).
   
```

In the shell:

```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> Server started on port: 33133
Server: result is 6
Client: received 6
```



