I used `mail.com` and `google.com` as the hosts for testing--they both redirected.  At some point, I decided to convert everything to `HTTP/1.1`.  HTTP/1.1 _requires_ a Host header (and I found that even HTTP/1.0 wouldn't work correctly without a Host header).  

In addition,  HTTP/1.1 creates a persistent TCP connection in order to avoid the overhead of setting up a TCP connetion everytime the client makes a request. The problem with that state of affairs is that the only way\** the client knows that it has read the entire response is if the server closes the socket.  

> HTTP/1.1 defines the "close" connection option for the sender to signal that the connection will be closed after completion of the response. For example,
>
>       `Connection: close`
>       
> in either the request or the response header fields indicates that the connection SHOULD NOT be considered 'persistent' (section 8.1) after the current request/response is complete.


So I included a `Connection: close` header in the request.  

Finally, I noticed used the `ssl module to open an ssl socket.  The problem was that mail.com redirected to an `https` url. 

http://erlycoder.com/89/erlang-ssl-sockets-example-ssl-echo-server-ssl-client-





