#lang typed/racket

(: x Number)
(define x 10)

(: a-list (Listof Number))
(define a-list (list 10 11))

(define-syntax push 
  (syntax-rules ()
      [(push item loc)
       (set! loc (cons item loc))]))

(struct: (a b) State-result ([val : a] [state : b]) #:transparent)
(struct: (b) State-error ([message : String] [last-state : b]) #:transparent)
(define-type (State a b) (U (State-result a b) (State-error b)))

(: state-return (All (a b) (a -> (b -> (State a b)))))
(define (state-return value)
  (lambda: ([state : b])
    (State-result value state)))

(: state-bind (All (a b) ((b -> (State a b))
                         (a -> (b -> (State a b)))
                         ->
                         (b -> (State a b)))))
(define (state-bind mv mf)
  (lambda: ([state : b])
    (match (mv state)
      [(State-result value state)
       ((mf value) state)]
      [(? State-error? e)
       e])))
       
(define-syntax state-do
  (syntax-rules (<-)
    [(state-do 
      (pat <- expr0) expr1 expr ...)
     (state-bind expr0 (lambda (pat)
                         (state-do expr1 expr ...)))]
    [(state-do
      expr0
      expr1
      expr ...)
     (state-bind expr0
                 (lambda (id)
                   (state-do expr1 expr ...)))]
    [(state-do
      expr)
     expr]))

(: push* (Number -> ((Listof Number) -> (State-result Number (Listof Number)))))
(define (push* x)
  (lambda: ([state : (Listof Number)])
    (State-result x (cons x state))))

(: pop* ((Listof Number) -> (State-result Number (Listof Number))))
(define (pop* state) (State-result (first state) (rest state)))
                         