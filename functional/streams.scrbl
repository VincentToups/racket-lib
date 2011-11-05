#lang scribble/manual 

@(require (for-label racket))

@title{Streams}

@defmodule{functional/streams}

This library provides support for lazy lists, aka "streams".
It provides the basic functions you need to construct streams
and a monad advanced stream comprehensions.

@section{Usage}

This is how we do it:

@subsection{Basics}

A stream is a data structure which is either the empty stream,
@racket[the-empty-stream], or a @racket[stream] structure,
which is a pair with a head, a value of any kind, and a tail,
which is a Racket @racket[promise], produced with the 
@racket[delay] form.  @racket[Delay] results in a promise,
which is basically just a thing which will evaluate into
a value, later, when you call @racket[force].  But the point
of building streams is so you don't have to think about this stuff.

You create a stream with @racket[stream-cons].

@racketblock[
             (require functional/streams)
             (define one (stream-cons 1 the-empty-stream))
             ]

This is a stream with just the value @racket[1] in it.  So:

@racketblock[
             (stream-head one)]

Will be one.

@racketblock[
             (stream-tail one)
             ]
             
Will be the empty stream.  Incidentally, the empty stream 
is itself a promise that produces the empty stream.  This behavior
may be rethought in the future.   

More interesting streams can be built with @racket[stream-cons]:

@racketblock[
             (define ones (stream-cons 1 ones))
             ]

This produces an infinitely long stream of the value 1.

@racketblock[
             (stream-head ones)
             (stream-head (stream-tail ones))
             (stream-head (stream-tail (stream-tail ones)))
             ...]

All evaluate to one.  How does this work?

@racket[Stream-cons] is a special form, rather than a function.
It automatically wraps the contents of the second argument in
a @racket[delay].  Eg:

@racketblock[
             (define ones (stream-cons 1 ones))
             (define ones* (stream 1 (delay ones*)))
                                               
             ]
             
Are equivalent pieces of code.

@section{Functions, Special Forms and Values}

@defstruct*[stream ([head any/c]
                    [tail-promise (or/c promise? stream-empty?)])]

Represents a non-empty stream.  This library redefines the 
@racket[stream?] predicate created by this structure definition
to return true when called on the empty stream as well as
stream structures.

@defthing[the-empty-stream stream-empty?]

Represents a stream with no values in it.  When using @racket[match],
match against the empty stream with @racket[(? the-empty-stream)].  

@defproc[(stream-empty? (s stream?)) (b boolean?)]

Returns true if @racket[s] is the empty stream, false if s is
a stream which isn't empty, and will produce an error if
called with a non-stream.

@defproc[(stream-tail (s stream?)) (result stream?)]

Returns the result of @racket[force]ing the tail-promise of the 
stream @racket[s], unless @racket[s] is the empty stream, in
which case, this produces an error.

@defform[#:id stream-cons (stream-cons head future)]

Constructs a stream, automatically wrapping the expression
@racket[future] in a @racket[promise] with @racket[delay]. 

@defthing[ones stream?]

The stream of ones.

@defthing[zeros stream?]

The stream of zeros.

@defproc[(stream-from (n number?)) (s stream?)]

This function returns a stream which starts at 
n and increases by one perpetually.

@defproc[(stream-down-from (n number?)) (s stream?)]

This function returns a stream which starts at n and decreases
by one perpetually.

@defproc[(stream-range (start number?) (incr number?) (terminal number?)) (s stream?)]

Produces a stream over a range of numbers. The stream starts with @racket[start],
increments by @racket[incr], and never exceeds or reaches the value @racket[terminal].

If @racket[incr] is negative, the stream goes down and never falls below 
or equals the value @racket[terminal].

@defproc[(take (s stream?) (n number?)) (l list?)]

Convert a stream into a list of @racket[n] elements (or fewer, if the
stream is shorter) by taking the first @racket[n] elements and 
placing them in a list.

@defproc[(stream-map (f procedure?) (s stream?) ...) (s stream?)]

Takes a function of N arguments and N streams and produces a new
stream which contains @racket[(f stream-el1 ... stream-eln)].

@defproc[(stream-zip (s stream?) ...) (s stream?)]

Return a stream of lists of elements of the input streams.
As long as the shortest input stream.

@defproc[(stream-append (s1 stream?) (s2 stream?)) (s stream?)]

Takes two streams and returns their concatenation.  The identity operator
when @racket[s1] is an infinite stream.

@defproc[(stream-interleave (s1 stream?) (s2 stream?)) (s stream?)]

Takes two streams and returns a stream which is their elements, interleaved.

@defproc[(stream-add-tail (s1 stream?) (tail promise?)) (s stream?)]

Produces a stream which has the elements of @racket[s1] but whose tail is
the stream produced by @racket[tail], a promise to produce
a stream.

@defproc[(stream-map-cat (f procedure?) (s stream?)) (s stream?)]

Takes a function @racket[f], which transforms an element of @racket[s] 
into a stream, and returns the concatenation of all these streams.

@defproc[(stream-map-interleave (f procedure?) (s stream?)) (s stream?)]

Takes a function @racket[f], which transforms an element of @racket[s]
into a stream, and returns the interleaving of all these streams.

@defproc[(stream-bind (s stream?) (f procedure?)) (s stream?)]

Same as @racket[stream-map-cat] with argument order reversed.  Bind
for the stream monad.

@defproc[(stream-bind* (s stream?) (f procedure?)) (s stream?)]

Same as @racket[stream-map-interleave] with argument order reversed.  Bind
for the interleaving stream monad.

@defproc[(stream-return (item any/c)) (s stream?)]

Returns a stream with one element, @racket[item].  

@defthing[the-stream-monad monad?]

The stream monad.

@defthing[the-stream-monad* monad?]

The interleaving stream monad.

@defproc[(list->stream (lst list?)) (s stream?)]

Returns a stream with the same elements as @racket[lst].  Lazy,
so that side effects on @racket[lst] will show up in the stream
when it is evaluated, except for the first element.

@defproc[(list->cycle (lst list?)) (s stream?)]

Returns a stream which has the elements of @racket[lst] repeating forever.

@defproc[(stream-of (value any/c)) (s stream?)]

Return an infinite stream of @racket[value].

@defproc[(stream-of-random-ints (z integer?) (w integer?)) (s stream?)]

Return a stream of random integers (32 bit) from a simple random
number generator seeded with @racket[z] and @racket[w].  Each
stream of numbers is repeatable.

@defproc[(stream-of-random-floats (z integer?) (w integer?)) (s stream?)]

Return a stream of random floats on the open interval (0,1).

@defproc[(stream-of-normals (z integer?) (w integer?)) (s stream?)]

Returns a stream of normally distributed numbers with mean zero and
standard deviation one.
