#lang scribble/manual

@title{Better Monads}
 
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


@section{Conclusions}

This should be enough to get you going.  Enjoy the monads!
