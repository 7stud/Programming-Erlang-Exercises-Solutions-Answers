This exercise took me a long time to complete.  I find erlang very hard to debug because of the poor error messages.  For instance, at one point I mistyped `HTTP` as `HTPP` in the request, and it took me forever to debug.  I eventually had to look at the binary output of my request and compare it byte for byte with a seemingly identical request that I knew worked.  

While working on the exercise, I found it very helpful to use `curl` to examine the format of the request and response:

     $ curl -vIF mail.com
     
which produces output like this:
```
$ curl -vIL mail.com
* Rebuilt URL to: mail.com/
*   Trying 74.208.122.4...
* Connected to mail.com (74.208.122.4) port 80 (#0)
> HEAD / HTTP/1.1
> Host: mail.com
> User-Agent: curl/7.43.0
> Accept: */*
> 
< HTTP/1.1 301 Moved Permanently
HTTP/1.1 301 Moved Permanently
< Date: Sun, 28 May 2017 21:44:57 GMT
Date: Sun, 28 May 2017 21:44:57 GMT
< Server: Apache
Server: Apache
< Location: https://www.mail.com/
Location: https://www.mail.com/
< Vary: Accept-Encoding
Vary: Accept-Encoding
< Connection: close
Connection: close
< Content-Type: text/html; charset=iso-8859-1
Content-Type: text/html; charset=iso-8859-1

< 
* Closing connection 0
* Issue another request to this URL: 'https://www.mail.com/'
*   Trying 74.208.122.4...
* Connected to www.mail.com (74.208.122.4) port 443 (#1)
* TLS 1.2 connection using TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384
* Server certificate: *.mail.com
* Server certificate: thawte SSL CA - G2
* Server certificate: thawte Primary Root CA
> HEAD / HTTP/1.1
> Host: www.mail.com
> User-Agent: curl/7.43.0
> Accept: */*
> 
< HTTP/1.1 200 OK
HTTP/1.1 200 OK
< Date: Sun, 28 May 2017 21:44:57 GMT
Date: Sun, 28 May 2017 21:44:57 GMT
< Server: Apache
Server: Apache
< Vary: X-Forwarded-Proto,Host,Accept-Encoding
Vary: X-Forwarded-Proto,Host,Accept-Encoding
< Set-Cookie: cookieKID=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 21:44:57 GMT; Path=/
Set-Cookie: cookieKID=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 21:44:57 GMT; Path=/
< Set-Cookie: cookiePartner=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 21:44:57 GMT; Path=/
Set-Cookie: cookiePartner=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 21:44:57 GMT; Path=/
< Cache-Control: no-cache, no-store, must-revalidate
Cache-Control: no-cache, no-store, must-revalidate
< Pragma: no-cache
Pragma: no-cache
< Expires: Thu, 01 Jan 1970 00:00:00 GMT
Expires: Thu, 01 Jan 1970 00:00:00 GMT
< Set-Cookie: JSESSIONID=03CBEC8498A0B2D36FCB9E38A5D1DDCE; Path=/mailcom-webapp/; HttpOnly
Set-Cookie: JSESSIONID=03CBEC8498A0B2D36FCB9E38A5D1DDCE; Path=/mailcom-webapp/; HttpOnly
< Content-Language: en-US
Content-Language: en-US
< Content-Length: 84952
Content-Length: 84952
< Connection: close
Connection: close
< Content-Type: text/html;charset=UTF-8
Content-Type: text/html;charset=UTF-8

< 
* Closing connection 1
```

#### Host header

I used `mail.com` and `google.com` as the hosts for testing--they both redirect.  At some point, I decided to convert everything to `HTTP/1.1`.  `HTTP/1.1` _requires_ a `Host` header, and I found that even `HTTP/1.0` wouldn't work correctly without a Host header.  I read something that said proxies may require the Host header.  

#### Connection header

`HTTP/1.1` creates a `persistent TCP connection` in order to avoid the overhead of setting up a TCP connection every time the client makes a request. The problem with that state of affairs is that the only way\** the client knows that it has read the entire response is if the server closes the socket.  

> HTTP/1.1 defines the "close" connection option for the sender to signal that the connection will be closed after completion of the response. For example,
>
>       `Connection: close`
>       
> in either the request or the response header fields indicates that the connection SHOULD NOT be considered 'persistent' (section 8.1) after the current request/response is complete.

So I included a `Connection: close` header in the request.  

#### https urls

I noticed that `mail.com` (unlike google.com) always redirects to an `https` url.  So I learned about ssl sockets, and I used the `ssl module` to open an ssl socket when the redirect was to an `https` url.  See here:

http://erlang.org/doc/apps/ssl/using_ssl.html

---

\** I discovered that HTTP/1.1 uses *chunked transfer encoding*.  Practically, what that meant for me was that the response hand hexadecimal numbers littered throughout.  The way that HTTP/1.1 persistent connections work is that the server sends data in chunks and preceding each chunk is the length of the chunk.  The tricky part of that is: the chunk the server sends gets split into smaller chunks when it gets transported across a TCP connection to the client, so only some of the chunks that the client reads have a Length at the start of the chunk.  Because the server does not close a persistent connection after sending the response, in order to indicate that the response has ended, the server sends 0 for the Length of the next chunk.  I decided not to try to deal with persistent sockets and reading the chunk lengths, so if you look at the body of the response in my output, the body is preceded by the hexadecimal chunk length `1ff8`.  The simple fix is just to change to HTTP/1.0 in my `format_request()` function.

---

My client requires that it be called with a full url.  That's because my client parses the url using `http_uri:parse()` and without the `http` or `https` part, parsing causes an error.  Edit: I added a fix for that.  Now, I can call client() with a url of the form: `mail.com`.

```erlang
-module(c2).
%%-compile(export_all).
-export([client/1, go/0]).

%%client(Url) ->
%%    {ok, Pieces} = http_uri:parse(Url),
%%    send_request(Pieces).

client(Url) ->
    case http_uri:parse(Url) of
        {ok, Pieces} ->
            send_request(Pieces);
        {error, no_scheme} ->
            client("http://" ++ Url);  %%Default to http (v. https)
        {error, Other} ->
            io:format("~p~n", [Other])
    end.

send_request({http, [], Host, Port, Path, Query}) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false},
                                         {packet,0}, {reuseaddr,true}]),
    Request = format_request(Host, Path, Query),
    io:format("Request:~n~s~n", [Request]),
    ok = gen_tcp:send(Socket, Request), 
    read(Socket, []);
send_request({https, [], Host, Port, Path, Query}) ->
    ssl:start(),
    {ok, Socket} = ssl:connect(Host, Port, [binary, {active, false},
                                            {packet,0}, {reuseaddr,true}]),
    Request = format_request(Host, Path, Query),
    ok = ssl:send(Socket, Request),
    read_ssl(Socket, []).

format_request(Host, Path, Query) ->
    %%Host header is required for HTTP/1.1, but HTTP/1.0 won't work without it either.
    %%Connection:close is needed because keep alive is the default in HTTP/1.1.
    HttpLine = io_lib:format("GET ~s~s HTTP/1.1", [Path, Query] ),
    HostHeader =  io_lib:format("Host: ~s", [Host]),
    UserAgent = "User-Agent: curl/7.43.0",  %%I used curl to examine the request: 
    Accept = "Accept: */*",                 %%   $ curl -ILv mail.com
    Connection = "Connection: close",       %%and I used the same mimimal headers that curl used.
    End = "\r\n",
    string:join([HttpLine,HostHeader,UserAgent,Accept,Connection,End], "\r\n").

read(Socket, Chunks) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Chunk} ->
            read(Socket, [Chunk|Chunks]);
        {error, closed} ->
            gen_tcp:close(Socket),
            Response = list_to_binary(lists:reverse(Chunks)),
            %%io:format("RESPONSE:~n~s~n", [JoinedBin]),
            handle_status(Response)
    end.

read_ssl(Socket, Chunks) ->
    case ssl:recv(Socket, 0) of 
        {ok, Chunk} ->
            io:format("~n****Chunk:*****~n~s~~n", [Chunk]),
            read_ssl(Socket, [Chunk|Chunks]);
        {error, closed} ->
            Response = list_to_binary(lists:reverse(Chunks)),
            handle_status(Response)
    end.

handle_status(Response)->
    [HeaderSection, _] = binary:split(Response, <<"\r\n\r\n">>),
    [HttpLine, Headers] = binary:split(HeaderSection, <<"\r\n">>),
    io:format("HttpLine:~n~s~nHeaders:~n~s~n~n", [HttpLine, Headers]),
    [_,Status|_] = binary:split(HttpLine, <<" ">>, [global]),
    StatusCode = list_to_integer(binary_to_list(Status)),
    io:format("StatusCode: ~w~n~n", [StatusCode]),
    handle_status(StatusCode, Headers, Response).

handle_status(Code, _, Response) when Code >= 200, Code < 300 ->
    io:format("***Response:***~n~s~n~n", [binary:part(Response, 0, 1000)] );
handle_status(Code, Headers, _) when Code >= 300, Code < 400 ->
    HeaderList = binary:split(Headers, <<"\r\n">>, [global]),
    handle_status_300(HeaderList, Code);
handle_status(Code, _, _) ->
    {error, {status_code, Code, no_handler}}.

handle_status_300([Header|Headers], Code) ->
    [Name, Value] = binary:split(Header, <<": ">>),
    case Name of
        <<"Location">> ->
            io:format("Location: ~s~n~n", [Value]),
            RedirectUrl = Value,
            client(binary_to_list(RedirectUrl));
        _ ->
            handle_status_300(Headers, Code)
    end;
handle_status_300([], Code) ->
    {error, {status_code, Code, no_location}}.

go() ->
    %%client("https://www.mail.com").
    %client("mail.com").
    %%io:format("~s~n", [ binary:part(Bin, {0,}) ]).
     
    %%client("http://www.google.com").   
    client("http://www.mail.com").
    
%%    Headers = [
%%        "GET / HTTP/1.1",
%%        "Host: www.google.com",
%%        "User-Agent: curl/7.43.0",
%%        "Connection: close",
%%        "Accept: */*",
%%        "\r\n" 
%%    ],
%%    Host = "www.google.com",
%%    Request = string:join(Headers, "\r\n"),
%%    client(Host, Request).
%%

```

In the shell:
```
$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.2  (abort with ^G)

1> Request:
GET / HTTP/1.1
Host: www.mail.com
User-Agent: curl/7.43.0
Accept: */*
Connection: close


HttpLine:
HTTP/1.1 301 Moved Permanently
Headers:
Date: Sun, 28 May 2017 12:57:22 GMT
Server: Apache
Location: https://www.mail.com/
Vary: Accept-Encoding
Content-Length: 229
Connection: close
Content-Type: text/html; charset=iso-8859-1

StatusCode: 301

Location: https://www.mail.com/

HttpLine:
HTTP/1.1 200 OK
Headers:
Date: Sun, 28 May 2017 12:57:23 GMT
Server: Apache
Vary: X-Forwarded-Proto,Host,Accept-Encoding
Set-Cookie: cookieKID=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 12:57:23 GMT; Path=/
Set-Cookie: cookiePartner=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 12:57:23 GMT; Path=/
Cache-Control: no-cache, no-store, must-revalidate
Pragma: no-cache
Expires: Thu, 01 Jan 1970 00:00:00 GMT
Set-Cookie: JSESSIONID=AD9CB6103185A8A0ADD619B8754E2717; Path=/mailcom-webapp/; HttpOnly
Content-Language: en-US
Connection: close
Transfer-Encoding: chunked
Content-Type: text/html;charset=UTF-8

StatusCode: 200

***Response:***
HTTP/1.1 200 OK
Date: Sun, 28 May 2017 12:57:23 GMT
Server: Apache
Vary: X-Forwarded-Proto,Host,Accept-Encoding
Set-Cookie: cookieKID=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 12:57:23 GMT; Path=/
Set-Cookie: cookiePartner=kid%40autoref%40mail.com; Domain=.mail.com; Expires=Tue, 27-Jun-2017 12:57:23 GMT; Path=/
Cache-Control: no-cache, no-store, must-revalidate
Pragma: no-cache
Expires: Thu, 01 Jan 1970 00:00:00 GMT
Set-Cookie: JSESSIONID=AD9CB6103185A8A0ADD619B8754E2717; Path=/mailcom-webapp/; HttpOnly
Content-Language: en-US
Connection: close
Transfer-Encoding: chunked
Content-Type: text/html;charset=UTF-8

1ff8
<!DOCTYPE html>
  <html lang="en" class="no-js">
  <head>
  <meta charset="utf-8" />
<meta http-equiv="X-UA-Compatible" content="IE=edge" />
<meta name="viewport" content="width=device-width, initial-scale=1">


<meta name="msapplication-tap-highlight" content="no" />
<meta property="og:title" content="Free email accounts | Register
```
