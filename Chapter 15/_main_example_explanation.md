### Explanation of C code.

First note that when you specify `{packet, 2}` as the port option, see p. 240, (or as a TCP socket option in chapter 17), erlang automatically calculates the Length of the message that you are sending, then prepends two bytes containing the Length to the message.  You have to assume that when you send a message, the message will get split up into an indeterminate number of chunks of indeterminate size.  Thus the other side needs some indicator to know when to stop reading because it has received the whole message.

I'm also going to use some pseudo code mixed in with the C code in my explanations.

```read_exact([], 2)```

That tries to read two bytes into the empty array, where the two bytes represent the Length of the message sent by the erlang code.
```
                buf  (for buffer, i.e. an array to store bytes)
                 |
                 V
     i = read(0, [], 2)
```
That reads from `stdin`, which is identified with a 0, and inserts the bytes read into the empty array. `2` is the number of bytes to read.  `buf+got` is pointer arithmetic and moves the `buff` pointer to a new spot in the array marking the end of the data read so far.  `read()` returns:

1. The number of bytes read, or
2. 0 if read() hits end-of-file, or
3. a negative number if read() encounters an error.


The trickiest bit is in the function ```read_cmd([)```:

    len = (buff[0] << 8 ) | buff[1]);
    
When you use the left bitshift operator on buff[0], which is an unsigned char (or one byte long), the bits are _not_ shifted off the left end leaving 0.  Rather, buff[0] is first converted to an int type (probably 4 bytes long), then the bits are shifted left.  An example to illustrate how the bitshifting(<<) and bit OR'ing (|) works is probably best.  Suppose the length of the message sent to the C code is 258.  That means the two bytes in buff will look like this:

     buff = [0000 0001, 0000 0010]  
     
Note that `0000 0001 0000 0010` is 256, but erlang split the length into two single bytes.  When you specify `{packet, 2}`, erlang automatically calculates the Length of the message you are sending, then prepends two bytes containing the Length of the message to the beginning of the message.  The other side can then read the first two bytes from stdin to get the Length of the message, then stop reading when it has received Length bytes.

The question is how do you convert buff[0], which is the binary representation for 1, and buff[1], which is the binary representation for 2, back into 258?

     0000 00001 << 8   ==>  ... 0000 0001 0000 0000  (That is 256.)

Then if you OR that result with buff[1]:

    len = ... 000 0001 0000 0000 | buff[1]);

first buff[1] will automatically be converted to an int, then the OR'ing will be done:

```
... 0000 0001 0000 0000
|   
... 0000 0000 0000 0010
--------------------
... 0000 0001 0000 0010
```

As a result, you end up combining two single bytes into an int containg the original length of 258.  

`write_cmd()` does the reverse and converts an integer, representing the length of the message containing the result, into two single bytes:

     first_byte = (len >> 8) & 0xff;

Suppose once again that the length (len) of the message to be written to stdout is 258 bytes:

     len = .... 0000 0001 0000 0010
     
This time bits _are_ shifted off the end:

     ... 0000 0001 0000 0010  >> 8    ==>  ... 0000 0000 0000 0001
       

Now when you AND that result with 0xff:

```
... 0000 0000 0000 0001
&
... 0000 0000 1111 1111
------------------------
... 0000 0000 0000 0001

```

AND'ing with 0xff effectively zeros out the bits to the left of the first byte while retaining all the ones in the first byte.  Finally, assigning that result to an unsigned char type, which is one byte long, serves to truncate the leftmost bits, giving you:

    first_byte = 0000 0001;

To get second byte for the length of the message, the example code then AND's the original length with 0xff:

    second_byte = len & 0xff
 
The AND'ing accomplishes this:

```
    ... 0000 0001 0000 0010
  & ... 0000 0000 1111 1111
  -------------------------
    ... 0000 0000 0000 0010
```
Then assigning that int to an unsigned char type truncates the int:

    0000 0010
    


			   
                     

    
  

