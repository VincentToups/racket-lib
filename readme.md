Racket Lib
----------

This is my racket library.  Nothing too interesting here yet, just a
few things to make me feel comfortable in this Lisp.

`functional/monads` provides a few simple monads and a `mlet*` for for
simulating Haskell `do` notation.  This is pretty similar to my
implementation of monadic binding in Emacs.

`utilities/fancy-destructuring` provides a clojure-like destructuring
bind suite with `dlet*`, `defn` and `fn`.  This tries to be more
Scheme flavored than the Elisp version, and its built up on top of
hygienic macros.

This implementation of destructuring bind can bind symbols, lists, or
racket "dicts" in the `racket/dict` module.  It looks like this:

    (dlet* ((x 10)) x)

`x` here is bound to ten.  When sumbols appear on the right hand side
of each binding expression, `dlet*` is identical to `let*`.  However,
`dlet*` extends the binding abilities of `let*` to include
destructuring.  For example:

    (dlet* (((: a b c) (list 1 2 3)))
       (+ a b c))

is `6`.  The `:` sigil indicates that we want to destructure a list
into three variables `a`, `b`, and `c`.  Destructuring can be nested
arbitrarily.  We can destructure a list within a list as so:

    (dlet* (((: (: a b) c) (list (list 1 2) 3)))
       (+ a b c))

is also `6`.

`fancy-destructuring` also supports destructuring anything which is a
`dict` in accordance with Racket's `racket/dict` library.  This covers
structs, various kinds of hash tables, and association lists.  Dict
destructuring is introduced with the `:>` sigil:

    (dlet* (((:> x 'a y 'b) '((a . 10) (b . 11))))
       (list x y))

evaluates to: `(10 11)`.  You can nest destructuring expressions for
lists or tables anywhere a symbol is expected above.  

If an item is not in the data structure to be destructured, then the
variable corresponding to that item is bound to `unbound`.

It is useful to have a way of binding where default values are
supported.  This library uses decorated sigils to accomplish this
task:


    (dlet* ((((: or '(1 2 3)) a b c)
             (list 10 11)))
      (list a b c))

evaluates to: `(10 11 3)` because the provided list is too short to
provide all the bindings.  

It is also useful to bind the structure itself occasionally.  This is
supported with the `as` sigil decoration:

    (dlet* ((((:> as table) a 'x)
            '((x . 10))))
     (list table a))

evaluates to `( ((x . 10)) 10)`.  `as` causes the input table to be
bound directly to the provided name.  `as` and `or` can be used
together and specified in any order.

Binding for Procedures
----------------------

It is nice to have this destructuring for function definition argument
lists, so the library provides `defn` and `fn` forms which use these
features to destructure their arguments.  

    (defn (add-three-list (: a b c))
     (+ a b c))

Takes a list with at least three arguments and adds them together.
Pretty useful!




