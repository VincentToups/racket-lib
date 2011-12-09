#lang racket

;;; We'll need Racket's mutable pairs.

(require racket/mpair)

;;; Our initial interpter will not be very discriminating: it is
;;; sufficient to recognize atoms, that is, values which evaluate to
;;; themselves, by:

(define (atom? x) (not (pair? x)))

;;; However, atoms as defined above include both symbols and things
;;; like strings and numbers.  We will be using symbols to refer to
;;; variables in our _interpreted_ code, though, so we need to draw a
;;; distinction between symbols and other atoms.

(define (atom/symbol? x) (symbol? x))
(define (atom/non-symol? x) (and (atom? x) (not (symbol? x))))

;;; I'd like to extend my interpreter as I go, because I think that is
;;; clearer.  We will take the following approach to allow this:

(define (suffix item onto)
  (reverse (cons item (reverse onto))))

(define-values (interpret extend-interpreter!)
  (let* ((interpreter-handlers '())
		 (extend-interpreter!
		  (lambda (dispatch-function handle-function)
			(set! interpreter-handlers
				  (suffix (cons dispatch-function handle-function) interpreter-handlers))))
		 (interpret
		  (lambda (exp env)
			(let loop 
				[(interpreter-handlers interpreter-handlers)]
			  (match interpreter-handlers
				[(list) (error (format "Can't interpret ~a - no handler matched!" exp))]
				[(cons (cons dis han) rest)
				 (if (dis exp) (han exp env)
                     (loop rest))])))))
	(values interpret extend-interpreter!)))

;;; The above creates a closure over three-values, the
;;; interpreter-handlers, a list of pairs, a function
;;; `extend-interpreter!`, which allows us to add a form-recognizer,
;;; form-handler pair to this list (at the end, using `suffix`) and a
;;; function `interpret`, which does the actual interpretation.
;;;
;;; Interpret loops through the handlers, testing the form it is
;;; working on with the first function in each pair.  If the test
;;; succeeds, it interprets the expression with the second function in
;;; the pair.
;;;
;;; The downside to this approach is that we destructure our
;;; expressions twice - once during matching and once during
;;; interpretation.  The upside is that we can extend our interpreter
;;; as we go, clearly separating concerns.  This kind of interpreter
;;; is about as slow as it can be anyway, so it hardly matters.

;;; Now we can add our first interpretation rule:

(extend-interpreter!
 atom/non-symol?
 (lambda (val _) val))

;;; Each interpreting function takes the form to interpret and the
;;; environment.  However, things like "this" and 10 don't need an
;;; environment, so we give it a name which indicates our disdain for
;;; it, `_`.  We simply return what we were passed in. 

;;; Now a note on the environment, since we'll need it soon: since
;;; Racket lists are made of _immutable_ pairs, and we'll need to
;;; mutate our environment, we are going to use mutable cons cells in
;;; our list.  The global environment, however, is the empty one:

(define the-global-environment (list))

;;; It needn't itself be mutable - adding items to the global
;;; environment will be accomplished by `set!` on
;;; `the-global-environment`, rather than mutation of the list itself.

;;;  We want to support adding a binding to the environment from
;;;  "outside" the language, and extending the environment from
;;;  inside.  Respectively:

(define (add-global-binding name value)
  (if (symbol? name)
	  (set! the-global-environment 
			(cons (mcons name value) the-global-environment))
	  (error "Bindings must be to symbols.")))

;;; This adds a new mutable cons pair to the global environment,
;;; create with `mcons`.

(define (extend-environment env vars vals)
  (match (list vars vals)
	[(list (list) (list)) env]
	[(list (cons key subs-keys)
		   (cons val subs-vals))
	 (extend-environment 
	  (cons (mcons key val) env) subs-keys subs-vals)]))

;;; Note that `extend-environment` produces a _new_ environment
;;; without modifying the old one.  This is important.

;;; Allowing us to say:

(interpret "A string" the-global-environment)
(interpret 100 the-global-environment)

;;; These yield their first arguments, as they should.

;;; Now let's add a rule for symbols.  First we need to know how we
;;; will look the meaning symbols up:.  The simplest possible choice
;;; is that we represent the environment as a list of pairs; eg
;;;    '((x . 10) (y . 11))
;;;

(define (lookup s env)
  (match env
	[(list) (error (format "No binding for ~a." s))]
	[(cons (mcons key val) env)
	 (if (eq? key s) val
		 (lookup s env))]))

;;; `lookup` is exactly the function which evaluates symbols.

(extend-interpreter!
 atom/symbol?
 lookup)

(interpret 'x (list (mcons 'x 100)))

;;; Which evaluates to `test-value`.

;;; Now we can write the interpreter cases for special forms.  Lisp In
;;; Small Pieces supports only `quote`,`if`,`begin`,`set!`, and
;;; `lambda`.  `quote` first:

(define quote-expression? (match-lambda 
						   [`(quote ,exp) #t]
						   [_ #f]))

;;; The evaluation of a quoted form is the form itself.

(extend-interpreter!
 quote-expression?
 (lambda (s env) (second s)))

(interpret '(quote x) the-global-environment)

;;; Results in `x`, as it should.

;;; Now for `if`:

(define if-expression?
  (match-lambda 
   [`(if ,test ,true ,false) #t]
   [_ #f]))

;;; We restrict ourselves to ternary if.

(extend-interpreter!
 if-expression?
 (lambda (e env)
   (match e 
	 [`(if ,test ,true ,false)
	  (if (interpret test env)
		  (interpret true env)
		  (interpret false env))])))

(interpret '(if #t 'x 'y) the-global-environment)
(interpret '(if #f 'x 'y) the-global-environment)

;;; These expressions are `x` and `y` respectively. 
;;; If's interpreter only interprets the appropriate branch.

;;; Now we implement begin:

(define begin-expression? 
  (match-lambda 
   [(cons 'begin rest) #t]
   [_ #f]))

;;; Here we create a struct whose sole instance is the value returned
;;; by an empty begin statement.  We could also forbid such a statement.

(struct Empty-begin () #:transparent)
(define the-empty-begin (Empty-begin))

(define (interpret-begin e env)
  (match e
	[`(begin) the-empty-begin]
	[`(begin ,expr) (interpret expr env)]
	[(cons 'begin  (cons expr rest))
	 (interpret expr env)
	 (interpret-begin (cons 'begin rest) env)]))

(extend-interpreter! 
 begin-expression?
 interpret-begin)

;;; We can't really do anything interesting with begin without
;;; implementating side effecting expressions.  However:

(interpret '(begin 'a 'b 'c) the-global-environment)

;;; is 'c, just as we expected.

;;; Now for `set!`, which will allow us to update the environment from
;;; within the language itself.

(define set!-expression? 
  (match-lambda
   [`(set! ,(? symbol? var) ,expr) #t]
   [_ #f]))

;;; This is a side effect inducing form and we'll need to use side
;;; effects to simulate the result.  To match L.I.S.P., however, we
;;; will not allow `set!` to introduce new bindings, only update them.

(struct Set!-result () #:transparent)
(define set!-result (Set!-result))

(define (update! env key value)
  (match env
	[(list) (error (format "Can't find a binding ~a to update." key))]
	[(cons (and (mcons lkey val) pair) rest)
	 (if (eq? key lkey) 
		 (set-mcdr! pair value)
		 (update! rest key value))]))

;;; `set-mcdr!` does the work.  We search until we find the symbol
;;; requested, and then we mutate its pair.  It is an error to set a
;;; symbol without a binding.

(define (interpret-set!-expression e env)
  (match e
	[`(set! ,(? symbol? var) ,expr)
	 (let ((val (interpret expr env)))
	   (begin (update! env var val)
			  set!-result))]))

(extend-interpreter!
 set!-expression?
 interpret-set!-expression)

;;; We will define a test environment which has a binding for 'x

;;; And test our new form:

(interpret '(begin (set! x 100) x) (list (mcons 'x 10)))

;;; Finally, we write our implementation of lambda:

(define lambda-expression? 
  (match-lambda 
   [`(lambda ,(? list?) ,@(list body ...)) #t]
   [_ #f]))

(define (interpret-lambda e env)
  (match e
	[`(lambda ,(? list? vars) ,body ...)
	 (lambda values
	   (interpret
		`(begin ,@body)
		(extend-environment env vars values)))]))

;;; Lambda's are represented by lambdas, which is confusing, I know
;;; (wait till you see the continuation passing interpreter!).
;;; However, the way this works will be clear in the invokation word.
;;; Our lambda encloses its env, var and body lists and interprets the
;;; latter with the environment extended with the former.  It receives
;;; values to bind to these symbols when it is invoked.

(extend-interpreter! 
 lambda-expression?
 interpret-lambda)

;;; This allows us to write and evaluate lambda expressions but we
;;; can't do anything with them until we define the part of the
;;; interpreter which invokes functions.

(define invokation?
  (match-lambda 
   [(list e maybe-args ...) #t]
   [_ #f]))

(define (interpret-invokation e env)
  (match e
	[(list e maybe-args ...)
	 (let ((f (interpret e env))
		   (args (map (lambda (e) (interpret e env)) maybe-args)))
	   (if (procedure? f)
		   (apply f args)
		   (error "Non-function in application position!")))]))

;;; Invoke checks to make sure its first element is actually a
;;; function, and then evaluates the arguments.  It then passes those
;;; values to the function produced by evaluating the first argument.
;;; This function contains an internal call to `interpret` in the
;;; extended environment of the function.

(extend-interpreter!
 invokation?
 interpret-invokation)

;;; Ok, we should add some things to our environment so that we
;;; can actually do things.

(add-global-binding '+ (lambda args (apply + args)))
(add-global-binding '- (lambda args (apply - args)))
(add-global-binding '* (lambda args (apply * args)))
(add-global-binding '/ (lambda args (apply / args)))

(interpret '(lambda (a b c) (+ a (+ b c))) the-global-environment)

(interpret '((lambda (a b c) (- (+ a b) c)) 1 2 3) the-global-environment)

;;; Wheeeee!

;;; Look out for the next interpreter!  Code is available on [github]().



