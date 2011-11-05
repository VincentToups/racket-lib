#lang scribble/manual 

@(require (for-label racket))

@title{Better Monads}

@defmodule[functional/better-monads]

This library is an attempt to facilitate pure functional programming
by providing a set of functions and special forms for working with
monads.  It also provides the definition of several common monads
extended in such a way as to make their use in dynamic languages more
convenient.  The user can define their own monads for later use as
well.

@section{Usage}

What follows is a brief tutorial on the usage of this library.

@subsection{mlet*}

The most important special form in the library is the @racket[mlet*]
syntax, which is the "Schemeish" monadic binding form, roughly
equivalent to Haskell's "do" notation.

Monads allow you to extend the meaning of variable binding within an
@racket[mlet*] expression, and hence @racket[mlet*] is very similar to
@racket[let*], in that it takes a series of binding pairs and then
evaluates a body of expressions, as in a @racket[begin] form.

In fact, at the top level, @racket[mlet*] behaves identically to
@racket[let*]:

@racketblock[
             (require functional/better-monads)
             (mlet* ((x 10)
                     (y 11))
                    (+ x y))]

By default, @racket[mlet*] performs its bindings in whatever monad is
stored in the lexical variable @racket[current-monad], which Better
Monads exports as the identity monad (also available in the variable
@racket[the-identity-monad]).

One difference, which applies to all monads, is that Racket's built-in
"match" patterns can be substituted for the symbols in the binding
form.  For example:

@racketblock[
             (struct doublet (a b))
             (mlet* (((list x y) (list 1 2))
                     ((doublet q r) (doublet 100 101)))
                    (list x y q r))]

Will return @racket['(1 2 100 101)].

@subsection{Working with a Monad}

Besides extending @racket[let*] with pattern matching, the library
doesn't do much until you start using monads other than
@racket[the-identity-monad].  Alternative monads are introduced using
the @racket[in:] option in @racket[mlet*].  For instance:

@racketblock[
             (struct doublet (a b) #:transparent)
             (mlet* in: the-list-monad
                    ((x '(1 2 3 4))
                     (y '(a b c d)))
                    (return (doublet x y)))]

Will result in:

@racketblock[(list
 (doublet 1 'a)
 (doublet 1 'b)
 (doublet 1 'c)
 (doublet 1 'd)
 (doublet 2 'a)
 ...)]

And so on, to include all the combinations of the items bound to
@racket[x] and @racket[y], which is the behavior of
@racket[the-list-monad].

Note the use of the function @racket[return] in the body of the
@racket[mlet*] expression.  In the context of an @racket[mlet*]
expression, the symbosl @racket[bind], and @racket[return] are bound
to the appropriate functions in the specified monad.  If the monad
also defines @racket[plus] and/or @racket[zero], these will also be
appropriately bound.  Otherwise they will be bound to @racket[#f].  In
the lexical scope of an @racket[mlet*] expression, the
@racket[current-monad] will be bound to the monad specified after
@racket[in:].

@subsection{Defining Additional Monads}

The use may define her own monads by creating an instance of the
@racket[monad] structure.  For instance, we might define the "maybe"
monad like this:

@racketblock[
             (struct Just (val) #:transparent)
             (struct None () #:transparent)
             (define (maybe-return item)
               (Just item))
             (define (maybe-bind monadic-value
                                 monadic-function)
               (match monadic-value
                 [(Just val) 
                  (monadic-function val)]
                 [(None) 
                  monadic-value]))
             (define
               the-maybe-monad
               (monad maybe-bind maybe-return #f #f))]

Having done such, we may write:

@racketblock[
             (mlet* in: the-maybe-monad
                    ((y (Just 10))
                     (x (Just 11)))
                    (return (+ x y)))]

Which evaluates to @racket[(Just 21)].  We might also write:

@racketblock[
             (mlet* in: the-maybe-monad
                    ((y (Just 10))
                     (x (None))
                     (z (Just 13)))
                    (return (+ x y z)))]

Which will evaluate to @racket[(None)].

In this example, we bound the monad to a symbol, but monads may be
specified anonymously.

@subsection{Lagniappe (Bonus Features)}

It is often useful to bind values non-monadically during a monadic
expression.  These can be introduced using the @racket[is:] syntax.
For instance:

@racketblock[
             (mlet* in: the-list-monad
                    ((x '(1 2 3))
                     (q is: (+ x 10))
                     (y '(a b c)))
                    (return (list q y)))]

Will produce @racket[(list (list 11 a) (list 11 b) ...)].  And so on.
The symbol @racket[q] is bound as in a @racket[let] expression for all
expressions subsequent.  The binding symbol can also be a
@racket[match] pattern.

@section{Other Forms and Functions}

At the moment, the library provides a few extra functions and
features.

@subsection{monadic-do}

The form @racket[monadic-do] provides an alternative, more
Haskell-ish, monadic binding form.  Each expression (except the last)
in the body of a @racket[monadic-do] must be either a binding
expression:

@racketblock[(pattern <- monadic-value)]

A "letting" expression:

@racketblock[(pattern is: expression)]

Or an expression resulting in a monadic value.  The last expression in
a monadic-do form can't bind or let any variables, but must evaluate
to a monadic value.  This version of the form might be more useful for
monads like the state monad which have many monadic values for which
the resulting binding isn't important.

Eg:

@racketblock[
             (define (state-push value)
               (lambda (state)
                 (state-doublet 'no-one-cares (cons value state))))
             (define state-pop
               (lambda (state)
                 (state-doublet (car state) (cdr state))))
             
             (monadic-do in: the-state-monad
                         (state-push 10)
                         (state-push 13)
                         (y <- state-pop)
                         (state-push (+ y 100)))]
                         

That is, we don't care about the value returned by @racket[state-push]
for binding in subsequent expressions.  This form threads our values
through the monad, but lets us avoid the noise of providing dummy
variable names.  Bindings to actual symbols is indicated specifically
by @racket[<-] for monadic binding and @racket[is:] for regular
binding.

@subsection{Bundled Monads}

The library comes with several monads pre-defined.  We've seen most of
them already, but its worth remarking on a few peculiarities.

@subsubsection{The List Monad}

The list monad behaves just like the list monad in Haskell or any
other language, in that bind is essentially "map-cat".  However,
unlike in Haskell, the list monad in this library will "return"
non-list values if they appear where a list should be.  You can say,
eg:

@racketblock[
             (mlet* in: the-list-monad
                    ((x '(1 2 3))
                     (y 4))
                    (return (+ x y)))]

When "bind" encounters "4" either in a place where a monadic value or
monadic function should be, it replaces it with the "right" value
(either @racket[(list 4)] or @racket[(lambda (_) (list 4))],
respectively).  If you want to return a list, you must @racket[return]
it explicitely.  @margin-note{Being able to squeeze values into our
monad like this is a nice benefit of working with monads in a language
with run-time type-checking.}
                    
@subsubsection{The State Monad}

The state monad is useful for constructing functions of state out of
smaller state functions.  Monadic values in this monad are functions
which take a state and return a @racket[state-doublet] struct.  The
first part of this doublet is the "proper return value" of the state
function, which is the value bound in binding expressions.  The second
value is the modified state.

Like @racket[the-list-monad], @racket[the-state-monad] makes an
attempt treat unexpected values correctly.  Literals are
@racket[return]ed into the monad if they appear where a monadic value
should be.  Functions can return a simple value, rather than a
state-doublet, in which case the bind assumes that the intent was to
simple insert that value into the monad without modifying the
accumulating state.

State functions may also return either a @racket[state-fail] struct or
a @racket[(state-error error-value)] struct to indicate failure.  Such
return values short-circuit all further state function bindings.  For
the state monad, then, @racket[state-fail] is the monadic zero and
@racket[state-error] allows the monad to report errors.

@subsection{Other Utility Functions/Forms}

It is often convenient to "lift" a function into a monad.  This is
provided for by a suite of lift functions.  Because functions have
variable arity in Racket, you must specifiy the number of arguments to
lift over, although this can be specified at run time (unlike the
Clojure library), up to 20 arguments.

Eg:

@racketblock[
             (define list+ (lift 2 + the-list-monad))
             (list+ '(1 2 3) '(4 5 6))]

Short-cut functions of the form @racket[lift1],@racket[lift2] and so
on are also exported.

The library also exports @racket[mapm] and @racket[foldlm].  The
former takes a monadic function and a list of values and returns a
monadic function which returns the list resulting from monadically
binding those values through the monad.

The latter takes a monadic function, an initial state, and a list of
values and returns a monadic function which folds over those values in
the monad, returning a monadic value which is the result of that
folding.  The function @racket[reducem] is the same, but assumes the
initial value is the @racket[car] of the list and folds over the
@racket[cdr].

@section{Functions/Values}

@defproc[(bind [monadic-value any/c] [monadic-function any/c])
         [monadic-value any/c]]

Binds a @racket[monadic-value] to the free variable in the calculation
represented by the @racket[monadic-function].  User defined binds might
coerce either argument into an appropriate form, hence the weak contracts.

@defproc[(return [item any/c])
                              (monadic-value any/c)]

Inserts @racket[value] into the currently operating monad.

@defthing[zero any/c]

The currently defined monadic-zero, @racket[#f] if the zero is not defined.  

@defproc[(plus [mval1 any/c] [mval2 any/c])
         (mval any/c)]

Combines two monadic values, if the operating is meaningful in the
current monad.  Will be @racket[#f] otherwise.

@defproc[(list-bind [lst (or/c list? any/c)] [list-fun (any/c . -> . (or/c list? any/c))])
         (lst list?)]

The bind operation for the list monad.  Applies the function @racket[list-fun] to
each item in @racket[lst], returns the concatenated result as a single list. 
Non-list values are wrapped in lists if encountered.

@defproc[(list-return [item any/c])
          (item-in-list list?)]

@defproc[(list-plus (a list?) (b list?))
         (c list?)]

The list-monad plus procedure, equivalent to @racket[append].

@defthing[list-zero list?]

Is the empty list.

@defthing[the-list-monad monad?]

The list monad.

The return operation for the list monad.  Puts @racket[item] into a list.

@defthing[the-identity-monad monad?]

The default monad, which does nothing.

@defproc[(state-return (item any/c))
         (state-function (any/c . -> .
                                state-doublet?))]
                                
The state monad return operation.  Creates a state function which 
leaves its input state unmodified and inserts @racket[item] into
the proper return value slot of a state doublet.

@defproc[(state-promote (object any/c))
         (state-function 
          (any/c . -> . 
                 (or/c state-doublet?
                       state-error?
                       state-fail?)))]

This function promotes non-monadic values into
monadic values in the state monad.  This is applied
to objects before being passed to bind, so that the
user can specify simple values instead of wrapping
them with return.

Handles doublets, errors and failures correctly.  "Returns"
all other values into the state monad.

@defproc[(state-promote-producer (object any/c))
         (state-function-function 
          (any/c . -> .
                 (any/c . -> .
                        (or/c state-doublet?
                       state-error?
                       state-fail?))))]

Similar to @racket[state-promote] but wraps
object into another function so that the result
is a monadic function.

@defproc[(state-bind [val (or/c
                           (any/c . -> . 
                                  (or/c
                                   state-doublet?
                                   state-error?
                                   state-fail?)))
                          any/c]
                     [fun
                      (or/c
                       (any/c . -> .
                              (any/c . -> .
                                     (or/c
                                      state-doublet?
                                      state-error?
                                      state-fail?))))])
         (any/c . -> .
                                     (or/c
                                      state-doublet?
                                      state-error?
                                      state-fail?))]

The state-monad bind operation.  Returns a new state function
which applies @racket[val] to the incoming state, extracts
the proper return value from the doublet, and creates and applies
a new state function to the new state.  If either a @racket[state-error]
or @racket[state-fail] is encountered, it is passed through.

@defproc[(state-plus [sf1 (or/c
                           (any/c . -> . 
                                  (or/c
                                   state-doublet?
                                   state-error?
                                   state-fail?)))]
                     [sf2 (or/c
                           (any/c . -> . 
                                  (or/c
                                   state-doublet?
                                   state-error?
                                   state-fail?)))])
         (sf (any/c . -> . 
                                  (or/c
                                   state-doublet?
                                   state-error?
                                   state-fail?)))]

Combines two state functions into a third, which applies @racket[sf1],
ignores its output, and then applies and returns the result of @racket[sf2].
Errors/failures are handled correctly.

@defthing[state-zero 
          (any/c . -> . 
                  state-fail?)]

The zero of the state monad.  Always returns a @racket[state-fail] structure.

@defproc[(lift [n (and/c integer? positive?)]
               [f procedure?]
               [monad monad?])
         (lf procedure?)]

Lifts @racket[f] into the monad @racket[monad].  The number of arguments
to lift must be provided in @racket[n].

@defproc[(mapm [monad monad?]
               [f procedure?]
               [lists list?] ...)
         (monadic-value any/c)]

Create a monadic value in the specified @racket[monad] by applying
f to each of the arguments specified in @racket[lists] in turn.  Return
a monadic value containing a list of the results.

@defproc[(foldlm [monad monad?]
                 [f (-> [item any/c] [acc any/c] [result any/c])]
                 [init any/c]
                 [lst list?])
         (monadic-value any/c)]

Returns a monadic value in the @racket[monad] obtained
by applying the monadic function @racket[f] iteratively
to the items in list and an accumulator.

@defproc[(reducem [monad monad?]
                  [f (-> [item any/c] [acc any/c] [result any/c])]
                  [lst list?])
         (monadic-value any/c)]

Like @racket[foldlm] but @racket[init] is taken to be
@racket[(car lst)] and the tail of the list is folded over.

@section{Structures}

@defstruct*[monad ([bind (any/c . -> . any/c)] [return (any/c . -> . any/c)] [plus (any/c . -> . any/c)] [zero any/c])]

The structure representing a monad.  

@defstruct*[state-doublet ([proper-return-value any/c] [state any/c])]

Represents a value/state pair for the state monad.

@defstruct*[state-error ([error-string (or/c string? any/c)] [last-state any/c])]

Represents a state-monad error condition.  The error string contains
information about the error, while @racket[last-state] is the last
state before the error.

@defstruct*[state-fail ()]

Completely dumb state error condition.  Provides no error information.
Is the state monad's zero.

@section{Syntax}

@defform[#:id with-monad (with-monad monad body ...)]

Introduces a context for the expressions in @racket[body ...] in which
the appropriate values, @racket[bind], @racket[plus], @racket[zero], @racket[return],
and @racket[current-monad] are bound to the values specified in @racket[monad].

@defform[#:id mlet* #:literals (in:) (mlet* in: monad [binding ...] body ...)]

Monadic binding expression.  Each binder must be either a @racket[(pattern value)]
pair, which binds and destructures monadically, or a @racket[(pattern is: value)] 
triple which binds and destructures non-monadically.  The body is finally evaluated,
returning the last value.  This is almost always necessarily a monadic value.

The @racket[in: monad] part may  be neglected, in which case the current lexical
@racket[current-monad] is used.

@defform[#:id monadic-do (monadic-do in: monad expr ...)]

Haskell-flavored monadic expression.  Each @racket[expr] must be either
a monadic binding expression, like @racket[(pattern <- value)] which introduces
the bindings specified by @racket[pattern] through the monad @racket[monad],
a regular binding expression like @racket[(pattern is: value)] which does
a plain bind, or an expression resulting in a monadic value.  The final @racket[expr]
must be a monadic value, not a binding form.

As in @racket[mlet*], the @racket[in: monad] may be neglected, in which
case the current lexically bound @racket[current-monad] is used.

@defform[#:id orm (orm in: monad expr ...)]

@racket[orm] is @racket[or] for monads.  Each @racket[expr ...] must
evaluate to a monadic value.  @racket[orm] @racket[bind]s the value in the initial
expression, and then checks to see if the value is true in the monad.  If
it is false, the next expression is bound.  If it is true, then
the form returns @racket[(return value)] for the monad and does not
evaluate subsequent expressions.  The expression @racket[orm in: monad e]
evaluates to @racket[e].  As usual, @racket[ in: monad ] can be
neglected, in which case the lexical @racket[current-monad] is used.

@defform[#:id andm in: monad expr ...]
Each @racket[expr ...] must
evaluate to a monadic value.  @racket[andm] @racket[bind]s the value in the initial
expression, and then checks to see if the value is true in the monad.  If
it is true, the next expression is bound.  If it is false, then
the form returns @racket[(return #f)] for the monad and does not
evaluate subsequent expressions.  The expression @racket[andm in: monad e]
evaluates to @racket[e].  As usual, @racket[ in: monad ] can be
neglected, in which case the lexical @racket[current-monad] is used.

@section{Conclusions}

This should be enough to get you going.  Enjoy the monads!
