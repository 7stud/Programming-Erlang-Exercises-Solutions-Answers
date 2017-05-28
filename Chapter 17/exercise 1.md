I used `mail.com` as the host, because it always redirected.  The problem was that the redirection was to an `https` url. So I converted everything to `HTTP/1.1`.  HTTP/1.1 _requires_ a Host header (and I found that even HTTP/1.0 wouldn't work correctly without a Host header).  In addition, the default for HTTP/1.1 is a persistent connection in order to avoid the overhead of setting up a TCP connetion everytime the client makes a request.

> HTTP/1.1 defines the "close" connection option for the sender to signal that the connection will be closed after completion of the response. For example,

       `Connection: close`
       
> in either the request or the response header fields indicates that the connection SHOULD NOT be considered 'persistent' (section 8.1) after the current request/response is complete.




The problem with that state of affairs is that the only way the client knows that it has read the entire response is if the server closes the socket.
