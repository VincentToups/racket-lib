#lang racket

(struct state-result (value new-state) #:transparent)
(struct state-error (error-value last-state) #:transparent)

(define (state-return value)
  (lambda (state)
    (state-result value state)))

(define (state-bind state-fun next-state-fun)
  (lambda (state)
    (let ((r (state-fun state)))
      (match r
        [(state-result value new-state)
         ((next-state-fun value) new-state)]
        [(? state-error?)
         r]
        [_ ((next-state-fun (void)) r)]))))

(define (state-plus f1 f2)
  (lambda (state)
    (match (f1 state)
      [(state-result _ new-state)
       (f2 new-state)]
      [(? state-error? error-value)
       error-value])))

(define-syntax build 
  (syntax-rules (<- :=)
    [(build 
      (pattern <- expr) rest0 rest ...)
     (state-bind 
      expr
      (lambda (id)
        (match id
          [pattern
           (build rest0 rest ...)])))]
    [(build 
      (pattern := expr) rest0 rest ...)
     (match expr
       [pattern (build rest0 rest ...)])]
    [(build expr0 expr1 expr ...)
     (state-plus
      expr0
      (build expr1 expr ...))]
    [(build expr) expr]))

(define-syntax named-build
  (syntax-rules ()
    [(named-build (name args ...)
                  expr ...)
     (letrec ((name
               (lambda (args ...)
                 (build expr ...))))
       name)]
    [(named-build name expr ...)
     (letrec ((name 
               (build expr ...)))
       name)]))

(define-syntax (let-build stx)
  (syntax-case stx ()
    [(let-build name 
                ((id expr) ...) 
                body0 body ...)
     (let* ((pairs (syntax->datum #'((id expr) ...)))
            (ids (map car pairs))
            (exprs (map cadr pairs)))
       (with-syntax 
           [((id ...) (datum->syntax (syntax body0) ids))
            ((expr ...) (datum->syntax (syntax body0) exprs))]
         #'(letrec 
               ((name (lambda (id ...) 
                        (build body0 body ...))))
             (name expr ...))))]
    [(let-build 
      ((id expr) ...) body ...)
     #'(let-build anon ((id expr) ...) body ...)]))

(define-syntax (build/let stx)
  (syntax-case stx ()
    [(build/let name 
                ((id expr) ...) 
                body0 body ...)
     (let* ((pairs (syntax->datum #'((id expr) ...)))
            (ids (map car pairs))
            (exprs (map cadr pairs)))
       (with-syntax 
           [((id ...) (datum->syntax (syntax body0) ids))
            ((expr ...) (datum->syntax (syntax body0) exprs))]
         #'(letrec 
               ((name (lambda (id ...) 
                        (build body0 body ...))))
             (name expr ...))))]
    [(build/let 
      ((id expr) ...) body ...)
     #'(build/let anon ((id expr) ...) body ...)]))


(define (the-state state)
  (state-result state state))

(define (set-state state)
  (lambda (_)
    (state-result (void) state)))

(define (do-nothing state)
  (state-result (void) state))

(define (state-false state)
  (state-result #f state))

(define (state-true state)
  (state-result #t state))

(define ==> state-return)

(define-syntax build/if 
  (syntax-rules (<-)
	[(build/if (<- expr) true false)
	 (build 
	  (id <- expr)
	  (if id
		  (build true)
		  (build false)))]
	[(build/if expr true false)
	 (if expr (build true)
		 (build false))]))

(define-syntax build/when 
  (syntax-rules (<-)
	[(build/when (<- expr) true ...)
	 (build 
	  (id <- expr)
	  (if id
		  (build true ...)
		  (==> (void))))]
	[(build/when expr true ...)
	 (if expr (build true ...)
		 (==> (void)))]))

(define-syntax build/unless
  (syntax-rules (<-)
	[(build/unless (<- expr) true ...)
	 (build 
	  (id <- expr)
	  (if (not id)
		  (build true ...)
		  (==> (void))))]
	[(build/unless expr true ...)
	 (if (not expr) (build true ...)
		 (==> (void)))]))

(define-syntax build/define 
  (syntax-rules ()
	[(build/define (name arg ...) body ...)
	 (define (name arg ...) (build body ...))]
	[(build/define name body ...)
	 (define name (build body ...))]))

(define-syntax build/lambda 
  (syntax-rules ()
	[(build/lambda args body ...)
	 (lambda args (build body ...))]))

(define-syntax build/cond 
  (syntax-rules (<-)
	[(build/cond ((<- expr) body ...))
	 (build 
	  (id <- expr)
	  (if id (build body ...)
		  (error "build/cond - no true condition!")))]
	[(build/cond (expr body ...))
	 (if expr 
		 (build body ...)
		 (error "build/cond - no true condition!"))]
	[(build/cond
	  ((<- expr) body ...)
	  term ...)
	 (build (id <- expr)
			(if id 
				(build body ...)
				(build/cond term ...)))]
	[(build/cond
	  (expr body ...) term ...)
	 (if expr 
		 (build body ...)
		 (build/cond term ...))]))

(define (state-and2 f1 f2)
  (build 
   (a <- f1)
   (if a 
	   (build 
		(b <- f2)
		(==> (and a b)))
	   state-false)))

(define (state-and . fs)
  (foldl (lambda (it ac)
		   (state-and2 it ac))
		 (car fs)
		 (cdr fs)))

(define (state-or2 f1 f2)
  (build 
   (a <- f1)
   (if a state-true f2)))

(define (state-or . fs)
  (match fs
	[(list f) f]
	[(cons f1 rest)
	 (build
	  (a <- f1)
	  (if a state-true 
		  (apply state-or rest)))]))

(provide build state-plus state-bind state-return 
         state-error state-result 
         named-build let-build
		 build/cond
		 build/if
		 build/when
		 build/define
		 build/unless
		 build/let
		 build/lambda
		 state-and state-or
         the-state set-state state-false state-true do-nothing ==>)
