In the Monads of Madness
------------------------

One thing about monad tutorials is that they almost universally cover
simple monads - often the most complex monad you get is the Sequence
monad.  The Error or Maybe monads are also common examples, but in
those dynamic languages which most folks are familiar with there are
already facilities for dealing with Errors or Exceptions and so these
monads seem like trinkets.  Why bother?

![Spirals](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure-5.png)
(Keep reading, I promise we will make pretty pictures.)

This seems to lead to the commonly expressed opinion that monads
aren't really useful in anything but Haskell, and that unless you live
there, there isn't any point in understanding them.  I can see this
perspective, particularly because unless you work in Lisp or want to
bang around with some heavyweight meta-programming tool like Metalua,
the syntax which makes using monads convenient isn't available
anyway.  

Another part of me, however, thinks its too bad.  There are some
problems which find elegant expression by recourse to monadic
computation.  Monadic parser combinators are an example, I think.  I'd
always found parsing to be an unapproachable activity, requireing
special use tools and an understanding of and separate domain specific
languages to program them.  If one develops monadic parser combinators
in Lisp, with the appropriate binding expressions to clean up the
monadic details, parsing pops out as just one more programming task.  

This post covers another example of a monad coming to the rescue when
a complex problem needs solving.  It is also an example of a monad
which I've never seen before (although it is some unnamed state
flavored monad transformer on the result of using the standard state
monad transformer on the sequence monad, I believe).  I'm going to try
and present things in the order they occured to me, and hopefully give
one a flavor of how one constructs a monad for a specific purpose,
rather than grabbing one off the shelf.  Also, for variety, we'll be
using Scheme, rather than Emacs Lisp.  While the code here is probably
a stonesthrow away from any Scheme implementation, I'm working in
Racket.

Remember Turtle Graphics?
-------------------------

Back when I was but a wee lad in Louisiana, I went to nerd camp.
This was a program called ADVANCE for kids who wanted to spend three
weeks of their summer in intense study - you could get credit to skip
highschool classes.  For brainy kids, this was also a place where you
could expect to be around a lot of other brainy kids, and if that
didn't exactly lift the pall of awkwardness which hung over most of
us, it surely made us feel less strange for it all.  

Anyway, I never took a programming class any of the summers I spent at
ADVANCE, but I do remember walking past the computer labs a couple of
times and seeing a little blinking triangle and a window beneath.
This is probably familiar to some other readers.  It was some
implementation of [Turtle Graphics][turtle-wikipedia].

In a Turtle Graphics system one is given a special programming
language with most of the usual fixings and one other distinguishing
feature: the language includes primitive operators for controlling the
motion of that little triangle on the screen - the titular Turtle.
You can tell the turtle to move forward or to rotate in place or to
lift up or place the "pen" it carries in its little forelimb.  When
the pen is down, the turtle leaves a line on the screen.

That first figure up there, for instance, was produced using the
library we'll build today.  The following code does it:

    (turtles-go
      (mlet* m-turtles
             ((new-pos (jump-to 150 150))
              (len     (setl 'len 2))
              (new-pos (n-times
                        (mlet* m-turtles
                          ((len (getl 'len))
                           (p   (move* len))
                           (r   (turn (/ pi 1.99)))
                           (len (setl 'len (+ len 2))))
                          (m-return p))
                        200)))
             (m-return new-pos)))

Here is the result again, since its not on the screen anymore:

![Spirals](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure-5.png)

You can imagine that with such a system, one can very quickly convince
the turtle to draw all sorts of spirographical images, telling the
turtle to repeat some sequence of commands which accumulate into
elaborate patterns.  Pretty neat stuff.

At first I wanted to create a turtle graphics system in a purely
functional (at least as purely functional as possible), as a learning
exercise.  But then I got to thinking:

What about a Hyperturtle?
-------------------------

Symmetry is an extremely important aesthetic principle which is
somewhat difficult to capture in Turtle Graphics.  It you want to
describe a system with bilateral symmetry using the turtle, you've got
to convince him to traverse the two sides of the figure in one go.
If you want to draw a Y, for instance, you have to say something like
(in pseudo code):

    (face north)
    (move 10)
    (rotate 45)
    (move 5)
    (rotate 180) ; these lines describe 
    (move 5)     ; backtracking 
    (rotate 90) 
    (move 5)

What if you could just move the Turtle up to the stem of the Y and
then tell it to rotate BOTH 45 degrees and -45 degrees and then tell
it to follow the rest of the instructions.  To satisfy your demands,
the Turtle must split itself into two turtles, but after that point
each turtle follows exactly the same directions, and you save yourself
a few commands and some complexity.

An even simpler approach might be to tell the turtle to have two
different "helicities," that is, one turtle interprets `(turn 45)` as
meaning to turn clockwise, and the other interprets it to mean
counter-clockwise.  Now any sequence of drawing commands will produce
a symmetric figure, all for free.  State dependent computations with
multiple possible outcomes?  

Is anyone else's monad sense tingling!?

Monads in Scheme (a diversion)
------------------------------

If you've read any of my [other posts about
monads][previous-monad-tutorial], you know how this is going to go.
We are going to define a macro to facilitate monadic computation.
Since this is a kind of advanced article, I'm going to asssume you
have the basic idea about `bind` and `return` more or less down.  If I
say that:

    (mlet* sequence-monad
      ((x '(1 2 3))
       (y '(4 5 6)))
     (m-return (+ x y)))

Expands to:

    (sequence-bind '(1 2 3) 
     (lambda (x)
      (sequence-bind '(4 5 6)
        (lambda (y)
          (sequence-return (+ x y))))))

You should follow what I mean and why.  As it happens, this monadic
binding expression is defined in the racket library associated with
this article, in `functional/monads`.  (Except there sequence-monad is
called `m-list`).  The `mlet*` defined in that library provides one
additional feature which I've found useful.  You can interpolate
monadic and non-monadic binding:

    (mlet* sequence-monad
      ((x '(1 2 3))
       (non-monadically: 
         ((q (+ x 1))))
       (y '(4 5 6)))
     (m-return (+ q y)))

Causes `q` to be bound only once per `x`, rather than bound as a
monadic value.  We're going to be liberally using `mlet*` throughout 
the rest of the article, so if you don't see how it works, check out
the other monad tutorial.

Turtle Functions
----------------

Let's backtrack and think about the regular old turlte monad.  A basic
turtle might be implemented as a dictionary containing keys like
`position`, `direction`, `pen-status`, etc.  In the land of
imperative, side-effect producing programs we'd probably just have a
global dictionary someplace holding these values.  In an object
oriented language we might do something obscene like a singleton
class.  Both would involve side effects.  Suppose we wanted to avoid
side effects forever (or at least until we had to talk to the hardware
handling rendering)?

The obvious solution is to thread our turtle state through tons of
functions which non-destructively modify it.  Our turtle may as well
carry around a representation of whatever "drawing" it has done in its
mind, too.  Once we pass the state through all of the state-dependent
functions we want to, we'll extract that representation of the drawing
and pass it on to our GUI to draw.  The modification of the memory of
the screen will be the only conceptual side effect.

What about querying the state?  We might want, for instance, to make
our turtle avoid certain regions of space, which means we'll need to
make inquiries into the state of the turtle, and we'd like to do so in
a standard way.  We'd also like to be able to combine queries with
modification.  What kind of function enables this behavior?

    (require racket/dict)
    
    (define (turtle-position^ turtle)
     (cons (dict-ref turtle 'position) turtle))
    
    (define (try-to-move turtle amt)
     (let* ((pos (dict-ref turtle 'position))
            (dir (dict-ref turtle 'direction))
            (new-position 
             (move-point-in-dir pos dir amt)))
      (if (point-on-screen? new-position)
          (cons new-position (dict-set turtle 'position new-position))
          (cons #f turtle))))

So, in order to let our turtle-dependent function communicate
information to us other than a possibly modified state value, we will
let them return a `pair` of things.  The first part of the pair is the
returned value, the second part is the possibly modified state.
`turtle-position^` just fetches the turtle's position into the first
part of the pair, returning the unmodified turtle in the second part
of the pair.  `try-to-move` actually uses the facility to return
values in a non-trivial way.  It calculates the new position of the
turtle, tests to see if it is on the screen and then returns the new
point and modified turtle if it is, and otherwise simply returns `#f`
and the unmodified turtle.  A caller of this function can inspect the
first value to see if it succeeded without knowing too much about the
internal representation of the turtle itself.  That's hott.

I want to point out something about the `try-to-move` function at this
point.  Note that it takes more arguments than just the turtle.
Somewhere down the line we might want to reduce it to a function of
just the turtle-state.  In fact, this is such a common thing to do
that we should really rewrite the function in a `curried` form:

    (define (try-to-move amt)
     (lambda (turtle)
       (let* ((pos (dict-ref turtle 'position))
              (dir (dict-ref turtle 'direction))
              (new-position 
               (move-point-in-dir pos dir amt)))
        (if (point-on-screen? new-position)
            (cons new-position (dict-set turtle 'position new-position))
            (cons #f turtle)))))

Now we've got a function which returns a compliant turtle function - a
function of just state and which returns a pair.  When we finally
figure out the machinery to combine such functions, we can just pass
in `amt` before we pass the resulting state function to that
machinery, where it can be handled in a uniform way.

Now that we've factored the functionality in this way, we can
recognize `try-to-move` as a function which produces a
turtle-function.  This is an important class of functions which we'll
call "turtle combinators."  Think of such a function as a turtle
function "waiting" for a value before it commits to being a particular
turtle function.  `try-to-move` represents the family of all movement
functions parameterized by `amt`.  Each call to `try-to-move` returns
such a function.

Thinking Monadically
--------------------

Ok, so we've cooked up an ad-hoc formalism for dealing with our purely
functional turtle.  Lets relate this to a monad.  

So one way of thinking about monads that comes up a lot is that there
is a monad for every data structure.  Lists form a monad, Trees form a
monad, the maybe monad is just a data type consisting of two kinds of
things `(Just <something>)` and `(None)` (in much the same way that a
list is either an element and a pointer to another list or nil).

We can imagine that a function of state which returns a value and a
new state is a kind of data structure.  The analogy isn't that
tortured, really.  You can put an element "into" a state function:

    (define (insert-into-state x)
      (lambda (state) 
        (list x state)))

Returns a state function that itself returns `x` when you ask for it
with a state.  You get data out of a state function by passing a state
in:

    (define (val-of pair) (car pair))
    (define (state-of pair) (cdr pair))
    (val-of ( (insert-into-state 10) '() )) -> 10

For the sake of driving the analogy home,

    (first (list 10)) -> 10

is the equivalent code for the data structure called a list.  We put
things in, and take them out again.  So if these state functions are a
data structure, what is the associated monad?  Welp, a monad exists
when you have a `return` and a `bind` operation.  For the list monad
these are:

    (define (list-return x) (list x))

    (define (reduce f lst)
      (foldl f (car lst) (cdr lst)))

    (define (list-bind monadic-val monadic-fun)
      (reduce append (reverse (map monadic-fun monadic-val))))

It should be reasonably clear why `insert-into-state` is
`state-return`.  It constitutes the operation of placing any object
into the monad.  What about bind?  Well, squint at the list-bind
operation and dig this: its essence is that it takes the monadic
function (a function which represents a monadic value "waiting" for
some input to compute itself) maps it over the values in the monad,
and collects each of them into a complete list.  Monadic functions
must take values and return monadic values, so the map returns a list
of lists, which we `reduce` with `append`.  If you squint, you can
pull out the following all right description for what bind does:

> Monadic bind takes values out of the monad, applies a monadic
> function to them, which results in lots of little monadic values.
> These monadic values are combined, finally, into one big monadic
> value, which is returned.

List-bind takes the elements out of a list, applies a function to them
to generate a bunch of lists, and combines them together.

What is the equivalent operation for the state monad?  Well, bind has
to return a monadic value, which is a state function in our case.  So
we know that bind will return a lambda.  And we know bind has to
extract the value from the state function passed in, and we know to do
that, bind will need the state value, so everything interesting has to
happen _inside_ the returned state function.  What has to happen in
there?

Well, we will apply the monadic value to the state passed into the
function we are returning.  This generates a pair, the first part of
which is the "value" extracted from the monad.  We'll pass this value
to the monadic function, which will generate a new state function.  We
will then pass the new state function the second part of the
previously produced pair, which is the new state value.  The return
value of the new state function on the new-state will be the return
value of the function we return.

Ok, I admit, that is a lot to follow.  Read the code:

    (define (state-bind state-fun state-fun-producer)
      (lambda (state)
        (let* ((pair (state-fun state))
               (val1 (val-of pair))
               (new-state (state-of pair))
               (new-state-fun 
                (state-fun-producer val1)))
          (new-state-fun new-state))))
    (define state-return insert-into-state)

It is slightly more obvious as source code than text.  That has got to
mean something about language, the power of abstraction, etc.  The
state monad is already implemented in `functional/monads`, so we can
write:

    (require functional/monads
             racket/dict)

    (define (st-fetch key . or-val)
      (if (not (empty? or-val))
       (lambda (state) 
        (list (hash-ref state key or-val) state))
       (lambda (state)
        (list (hash-ref state key) state))))

    (define (st-set key val)
      (lambda (state)
        (list val (hash-set state key val))))

    (define (point+ p1 p2)
      (map + p1 p2))

    (define (dir->vec dir)
      (list (cos dir) (sin dir)))
   
    (define (point* p scalar)
      (map (lambda (x) (* x scalar)) p))

    (define (move amount)
      (mlet* m-state
        ((position (st-fetch 'position '(0 0)))
         (direction (st-fetch 'direction 0))
         (non-monadically:
          ((new-position (point+ position 
                          (point* amount
                            (dir-vec direction))))))
         (_ (st-set 'position new-position)))
       (m-return new-position)))

Let's focus on the last function - `move`.  The body is a monadic
expression in the state monad, which means that it returns a function
itself.  Specifically, it returns a function which takes a state and
returns a value/state pair.  Each expression in the binding part of
the `mlet*` threads the value produced by each right hand expression
through state-bind and binds the variable in the left hand side to the
"value" part of the value returned by the state function in the
subsequent expressions.  The body of `mlet*` provides a final location
to evaluate expression in the monad.  It must evaluate to a state
function, hence we use `m-return`.  

The Hyperturtle Monad
---------------------

Our mask slipped above and we just started calling the turtle monad
the state monad, which is exactly what it is.  Astute readers might
notice that it is also, for all intents and purposes, equivalent to
the parser monad (types notwithstanding, obviously) seen from a
different perspective.  

The whole reason I'm writing this up is because I've never read a
really lengthy development of a _novel_ monad.  Most tutorials say
"you don't really have to understand how to write one, just how to use
them," but I think this is a shame.  As we'll see, the right monad
will enable an almost magical extension to the notion of turtle
graphics.

Ok, so lets think some about the hyperturtle we want to create.  The
basic idea is that our turtle can be told to do multiple things _at
once_.  That is, we want to be able to say something like

    (parallel-rotate (/ pi 2) (- (/ pi 2)))

And have our system interpret as a command which results in the turtle
splitting into _two_ turtles, one which rotates +pi/2 and the other
which rotates -pi/2.  In other words, we want to allow multiple
possible outcomes to each monadic function, which sounds a lot like
the list monad.  Monadic functions of the list monad take a single
value and return a list of values.  Depending on one's perspective, it
can be meaningful to view this list of results as a list of possible
return values over which subsequent computation in the monad is split:

    (mlet* m-list
     ((x '(1 2 3))
      (y (list (+ x 1) (- x 1))))
     (m-return (list x y)))

Will evaluate to:

    ((1 2) (1 0) (2 3) (2 1) (3 4) (3 2))

by this very logic.  For each possible value of `x` we have multiple
values of `y`.  We finally return all the combinations of these
possible bindings in a list of possible results.  

But the hyperturtle monad can't be the list monad alone.  After all,
each expression in the binding has to depend on the previous turtle
state, and the result of a monadic expression needs to be like a state
function somehow.  Turns out there is a standard way of constructing
the monad we are interested in using monad-transformers, but we'll do
it the hard way for now so as to elucidate how one goes about it.

Let's think carefully.  We still need state functions, but now they
are going to be returning "virtually" more than one possible outcome.
Since each possible outcome might also involve a separate modification
of the state, as well as a distinct value, it might make sense for our
functions to accept a single state but return a _list_ of value/state
pairs.  So, our monadic values are functions which take a state and
return a list of possible outcomes.

What does return look like for this hypothetical monad?  Well:

    (define (pstate-return val)
      (lambda (state)
        (list (pair val state))))

Bind is going to be a bit more difficult, but we can keep our head
straight by just remembering what bind always does.  It returns a new
monadic value, so its got to return a function which takes a state and
returns a list of pairs.  Inside that function, we've got to use the
state to produce a list of state/val pairs.  Then we need to use each
pair to produce the _next_ state function, which we apply to the
new-state to get a list of state/val pairs.  We collect all these
pairs back together into a single list of state/val pairs, which is
our return value.

Might be clearer in code:

    (define (pstate-bind pstate-fun pstate-fun-producer)
      (lambda (state)
        (let* ((pairs (pstate-fun state))
               (new-results
                 (map 
                   (lambda (pair)
                     (let ((val (car pair))
                           (state (cdr pair)))
                      ((pstate-fun-producer val) state)))
                   pairs)))
          (reduce append new-results))))

Ok, so there we go.  We've got a parallel state monad where we can
orchestrate multiple results.  But wait?  What if turtles need to
share information with one another or reference a "global" state?  For
instance, a turtle might wish to avoid an area if a line has already
been drawn there.  We need an additional level of _global_ state?  How
might we represent that?  

One way is to consider functions which accept a _pair_ of states, one
representing a possible "local" state and the other representing the
shared "global" state.  These functions want to reflect that the local
state might split into many new local states (or none at all), and
they also might modify the global state.  So its got to return the
list of new local states, the new global state and the local results
of each possible outcome.  The functions can sensibly return this
collection of things by returning a pair consisting of a list of
value/local-state pairs and a global state.  Consider the return
operation for this new monad:

    (define (ps-return val)
     (lambda (st-pair)
      (pair (list (pair val (car st-pair)))
            (cdr st-pair))))

We return a state function which destructures the st-pair input,
returning a pair containing a list with one element, a val/local-state
pair, and the global state.

Now the coup de grace, what is the find operation for this crazy
monad?  Well, it is similar to the bind operation for the
sequence-of-state monad, _except_ we have to deal with the global
state.  Each monadic value needs to use the previously calculated
global state function.  The named let expression below amounts to a
fold operation over the global state.

    (define (ps-bind monadic-val monadic-fun)
      (lambda (state-doublet)
        (let* ((results/global
                (monadic-val state-doublet))
          (let loop ((global-state (cdr results/global))
                     (pairs (car results/global))
                     (results-acc '()))
             (if (empty? pairs) (reduce append (reverse results-acc))
                 (let* ((pair (car pairs))
                        (rest (cdr pairs))
                        (val (car pair))
                        (new-mv (monadic-fun val))
                        (new-res (new-mv (cons (cdr pair)
                                   global-state))))
                  (loop (cdr new-res)
                        rest
                        (cons (car new-res) results-acc)))))))))

Ok, this is kind of complicated - but we've expressed it in just 17
lines of code.  Include return and we're up to 21 lines.  Once we
define a few "primitive" monadic functions/values we'll be able to
write incredibly concise and arguably expressive hyperturtle code.

Useful Hyperturtle Functions
----------------------------

We'll want to tell a branch of our computation to simply terminate.
This corresponds to returning no values whatever in the local part of
a state function, and leaving the global state unmodified:

    (define (poof st-doublet)
      (cons '() (cdr st-doublet)))

We're going to use the good old association list to represent our
local and global states.  That way we can add and remove arbitrary
data from them.  Just in case we want to use another table abstraction
later, though, we're going to use racket's dict library to wrap them:

    (require racket/dict)
    
    (define (setl key val) ; setl = set local
     (lambda (state-doublet)
       (cons (list (cons val
                         (dict-set (car state-doublet) key val)))
             (cdr state-doublet))))

    (define (setg key val) ; setg = set global
     (lambda (state-doublet)
       (cons (list (cons val (car state-doublet)))
             (hash-set (cdr state-doublet) key bal))))

Ok, so our pattern sense should be tingling.  We keep writing
functions which take some parameters now and return a state function.
Let's whip up some notation for this kind of common task.  Eventually
our library of combinators/monadic functions/monadic values will be
large enough that merely `mlet*` will be sufficient to write most
functions of interest, but until then, its nice to have some syntactic
sugar.  Consider the hygeinic macro:

    (require racket/match)

    (define-syntax (define<turtles> stx)
      (syntax-case stx (^)
        [(_ (id lstate-id gstate-id) body ...)
         (syntax (define (id dblt)
                   (match dblt
                     [(cons lstate-id gstate-id) 
                      (begin body ...)])))]
        [(_ (id lstate-id gstate-id ^ var) body ...)
         (syntax (define (id var)
                   (lambda (dblt)
                     (match dblt
                       [(cons lstate-id gstate-id)
                        (begin body ...)]))))]
        [(_ (id lstate-id gstate-id ^ var ...) body ...)
         (syntax (define (id var ...)
                   (lambda (dblt)
                     (match dblt
                       [(cons lstate-id gstate-id)
                        (begin body ...)]))))]
        [(_ id val)
         (syntax (lambda (dblt)
                   (match dblt
                     [(cons lstate-id gstate-id) 
                      (cons (list (cons val lstate-id)) gstate-id)])))]))
               
This lets us write the above two functions like so:

    (define<turtles> (setl local global ^ key val)
      (cons (list (cons val (dict-set local key val))) global))

    (define<turtles> (setg local global ^ key val)
      (cons (list (cons val local) (dict-set global key val))))

Ahhhhhh.  Much more concise.  For those not too familiar with
syntax-rules style macros, this:

    (define<turtles> (setg local global ^ key val)
      (cons (list (cons val local) (dict-set global key val))))

expands to:

    (define (setg key val)
     (lambda (state)
      (match state
       [(cons local global)
        (begin (cons (list (cons val local) (dict-set global key
     val))))])))

The `^` character is just a piece of visual lint meant to help
distinguish between the fact that the first two arguments are curried
off from the rest of the arguments.

We've now constructed enough machinery to understand the actual code
used in my implementation.  There is just one note I should make
before we switch over to looking at actual code.  Since we are
returning a list of pairs _and_ we're using association-lists, which
are just lists, after all, and lists are made of pairs, I felt that we
were just overburdening `cons` a little too much to make the code easy
to debug.  Hence, instead of using `pairs` where I wasn't using a
list, I created a new structure:

    (define doublet (a b))
    (doublet-a (doublet 10 11)) -> 10
    (doublet-b (doublet 'x 'y)) -> 'y

So with doublets, 

    (define<turtles> (setg local global ^ key val)
      (cons (list (cons val local) (dict-set global key val))))

Becomes

    (define<turtles> (setg local global ^ key val)
      (doublet (list (doublet val local) (dict-set global key val))))

This change gives the Racket environment a little hand when it comes
to checking types.  

I promise, things are about to get RAD.  We just need a few more
primitives:

    (define<turtles> (getg lo g ^ symbol)
      (doublet (list (doublet (dict-ref g symbol #f) lo))
            g))

    (define<turtles> (getl lo g ^ symbol)
      (doublet (list (doublet (dict-ref lo symbol #f) lo))
            g))

    (define<turtles> (getl-or lo g ^ symbol or-val)
      (doublet (list (doublet (dict-ref lo symbol or-val) lo))
            g))

    (define<turtles> (getg-or lo g ^ symbol or-val)
      (doublet (list (doublet (dict-ref g symbol or-val) lo))
            g))


These primitives let us fetch values from either the local state or
the global one.  

Ok, now here is the cool step.  Consider the function:

    (define<turtles> (split-setl lo g ^ key vals)
      (doublet 
       (map (lambda (val)
              (doublet val (dict-set lo key val)))
            vals)
       g))

This function lives in the turtle-state monad, evidently enough.  It
takes a key and a _list_ of values in `vals`.   It then returns a
_list of_ new local states where the key has been set to each of the
values in turn.  Any further state function when used within the monad
will now execute in multiple contexts, one for each possible
assignment of the `key`.  

Representing the Drawing
------------------------

All turtles will share a global representation of the drawing, which
we will store in a global `key` as a series of objects to draw.
Hence, the function to add a line to a drawing might be:

    (struct point (x y))
    (struct line (p1 p2))

    (define (add-line x1 y1 x2 y2)
      (mlet* m-turtles 
             ((lines (getg 'draw-these))
              (lines (setg 'draw-these (cons (line 
                                              (point x1 y1)
                                              (point x2 y2))
                                             (if lines lines '())))))
             (m-return lines)))

    (define (add-line-pts p1 p2)
      (mlet* m-turtles 
             ((lines (getg-or 'draw-these '()))
              (lines (setg 'draw-these (cons (line 
                                              p1
                                              p2)
                                             lines))))
             (m-return lines)))

These functions also provide an example of using the turtle monad with
monadic notation to write monadic functions.  In `add-line` we write a
function which takes four values and returns a state function produced
by the `mlet*` form using the `turtles-monad` (incidentally, this is:

    (define m-turtles
      (list (cons 'bind m-bind)
            (cons 'return m-return)
            (cons 'plus #f)
            (cons 'zero poof)))

).

Monads are simply dictionaries containing the appropriate operators.

`add-line` fetches the value of the global key `draw-these` or an
empty list, conses a new line onto that list, and finally sets the
value of that key with `setg`.  To finally draw our drawing, we'll
call our monadic function on an initial state (

    (define init-state (doublet '() '()))

), take apart the returned state doublet, grab the `draw-these` data
from the global state, and pass it to Racket's GUI library.  This
tutorial is already too long to provide all the code to do that, but
you can download it all from my github.  The interface is easy
enough.  The function called `turtles-go` takes a turtle-state fun and
applies it to an emtpy initial state, pulls out the drawing after the
function is finished, and draws it.

Let's practice using the turtle monad to draw something.  How about a
regular n-gon?  A regular n-gon is defined by an edge length and a
number of sides.  A regular n-gon's interior angles are all the same
and add up to (n-2)*pi radians, which means they must each be
((n-2)*pi)/n radians.  To draw such a figure, we can just move by and
edge length, rotate by the appropriate angle and repeat the process
until we reach the number of sides the polygon has.  

We'll need a move and a turn function:

    (define (turn amount)
      (mlet* m-turtles
             ((helicity (getl-or 'helicity 1))
              (facing (getl-or 'facing (/ pi 2)))
              (new-facing (setl 'facing
                                (+ (* amount helicity) facing))))
             (m-return new-facing)))

    (define (move amt)
      (mlet* m-turtles
             ((pos (getl-or 'pos (point 0 0)))
              (facing (getl-or 'facing (/ pi 2)))
              (non-monadically: 
               ((facing-vector 
                 (direction->vector facing))))
              (new-pos (setl 'pos (point+ 
                                   pos (scale facing-vector amt)))))
             (m-return new-pos)))

    (define (move-line amt)
      (mlet* m-turtles
             ((pos (getl-or 'pos (point 0 0)))
              (new-pos (move amt)))
             (add-line-pts pos new-pos)))

The turtle's current direction is stored in the local variable
`facing,` its current position stored in `pos`.  `turn` also uses a
value called `helicity` to determine _which way_ the turtle turns.
For now, we'll always use the default value of `1`.  `move-line` just
does the appropriate move and adds a line between the previous and
new-current point.

    (define (n-times fun n)
      (if (= n 1) fun
          (mlet* m-turtles
                 ((_ fun))
                 (n-times fun (- n 1)))))

`n-times` takes a monadic function and causes it to execute `n`
times.  Ok, here is our polygon drawing routine:

    (define (n-gon n edge-len)
      (let* ((int-ang (/ (* (- n 2) pi) n))
             (ext-ang (- pi int-ang)))
        (n-times
         (mlet* m-turtles
                ((p (move-line edge-len))
                 (a (turn ext-ang)))
                (m-return p))
         n)))

And here is how we use it:

    (turtles-go
     (mlet* m-turtles
            ((p (jump-to 150 150))
             (p2 (n-gon 6 30)))
            (m-return p2)))

And here is the result:

![polyfig](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure.png)

So we're still in regular old turtle territory.  Can we see an example
of hyper turtle action?  Well, what would you think the following will
produce?

    (turtles-go
     (mlet* m-turtles
            ((p (split-setl
                 'pos
                 (list (point 150 150)
                       (point 200 150)
                       (point 150 200)
                       (point 200 200))))
                 (p2 (n-gon 6 30)))
                (m-return p2)))

Instead of just `jump`ing `to` the single point `150 150` we've
instructed our turtle to jump _in parallel_ to four different points,
and then to cruise along drawing a polygon just as before.  What is
the result?

![polyfig2](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure-2.png)

We can do some pretty neat things in short order by changing the way
motion commands are interpreted by the turtle.  Remember the helicity
option?  Helicity changes the way individual turtles interpret turn
commands:

    (turtles-go
     (mlet* m-turtles
            ((p (split-setl
                 'pos
                 (list (point 150 150)
                       (point 200 150)
                       (point 150 200)
                       (point 200 200))))
             (h (split-setl
                 'helicity '(-1 1)))
              (p2 (n-gon 6 30)))
             (m-return p2)))

Results in:

![p3](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure-3.png)

We can do tons of things by modifying helicity, but lets do one more
different trick:

    (turtles-go
     (mlet* m-turtles
            ((p (jump-to 150 150))
             (n (split-setl
                 'n '(3 5 7 9 11 13)))
              (p2 (n-gon n 30)))
             (m-return p2)))

Results in:

![p4](https://s3.amazonaws.com/VincentToupsScreencasts/monad-turtles-polygon-figure-4.png)

Eee gads!  We've drawn the first six prime n-gons all at once!  I
leave you to consider what could be done with the following
combinator:

    (define (move* amt)
      (mlet* m-turtles 
             ((jitter-mag (getl-or 'jitter-mag 0))
              (non-monadically:
               ((amt (+ amt (random-normal 0 jitter-mag)))))
              (pos (getl-or 'pos (point 0 0)))
              (_ (move amt))
              (pos2 (getl 'pos))
              (f (getl-or 'move*-fun (lambda () add-line-pts)))
              (_ (f pos pos2)))
             (m-return pos2)))


Musings on Monads
-----------------

Besides drawing pretty pictures, what are we trying to accomplish
here?  Well, if you go sniffing around the internet about monads,
you'll often hear that monads "don't work" programming languages
besides Haskell.  Reasons proferred include "it is easier to use
side-effects," "there isn't syntactic support," and "you need static
typing."  I'm not a Haskell programmer, so I can't speak too
authoritatively on the subject static typing, but I think that at
least in Lisp dialects where the second argument can be easily
dispensed with, the first argument doesn't hold water.

If you look up the Forth pages on the C2 Wiki you'll eventually come
across the programmer's meme: [Do the Simplest Thing That Could
Possibly Work][c2-simple].  This means what it says: when you've got a
problem, don't worry about picking an optimal solution, just do the
simplest thing that could possibly work.  

In my mind, this was the simplest implementation of a hyperturtle
graphics system. 

Now, I know that is going to seem pretty crazy to a person just trying
to wrap their heads around monads.  Surely, you'll object, this can't
be the simplest possible implementation!  To that, I'd say go back and
look at the implementation of `return` and `bind` which together add
up to 21 or so lines of code.  If we had the right monad transformers
at our disposal, we could write the same monad in one line of code!
Assuming we had a handle on each of the monads in question, what we'd
have accomplished is the creation of a sub-langage with entirely novel
semantics in a few tokens.  Add the `mlet*` syntax (itself about 20
lines of syntax-case macro) and the minimal subset of `turtle`
primitive functions, and we are suddenly looking at a
domain-specific-langage _on steroids_.  

But this is a very simple solution!  You write a short bind and return
function, use the already "standard" monadic syntax, and inside those
expressions, you've got a whole new programming language.  Because
everything is purely functional, up to this point you've made _no hard
design decisions_.  You haven't declared any global variables, thought
about how to represent the persistent state of anything.  The monad
provides a clean bed to write code in - and you don't need much code
to get somewhere interesting.

Performance might be a problem - but I'd suggest that as a prototype
system the monad provides excellent semantics for any more efficient
implementation to target.  This is enhanced because of the very
compactness of the implementation!  You know what your system does
when you design it this way, because all the important behavior is
specified in one place.

Secondly, I hope the article has provided an example of the process by
which monads are constructed and used that isn't particularly
artificial.  Most monad tutorials provide ad-hoc examples which don't
do anything interesting, and use pre-fab monads.  I hope this
development of a free-form monad for a novel problem is interesting.  

And that is about it!

As always, the code for this project is hosted on [my
github][github-racket].  Comments, critiques, corrections are
extremely welcome!


* * *

[turtle-wikipedia]: http://en.wikipedia.org/wiki/Turtle_graphics
[c2-simple]: http://c2.com/xp/DoTheSimplestThingThatCouldPossiblyWork.html
[previous-monad-tutorial]: http://dorophone.blogspot.com/2011/04/deep-emacs-part-1.html
[github-racket]: https://github.com/VincentToups/racket-lib/blob/master/roguelikes/turtles-monad.md