#lang racket

(require (for-syntax syntax/parse)
         (for-syntax racket/match)
         racket/match
         syntax/parse)

(define (bind v f) (f v))
(define (return v) v)
(define zero #f)
(define plus #f)

(struct monad (bind return plus zero) #:transparent)

(define current-monad (monad bind return zero plus))

(define-for-syntax (sym-append . syms)
  (if (= 1 (length syms)) (car syms)
      (string->symbol (foldl 
                       (lambda (it ac)
                         (string-append ac "-" (symbol->string it)))
                       (symbol->string (car syms))
                       (cdr syms)))))

(define-syntax (with-monad stx)
  (syntax-parse 
   stx
   [(with-monad monad-expr:expr body:expr ...)
    (with-syntax
        ((current-monad (datum->syntax #'monad-expr 'current-monad))
         (bind (datum->syntax #'monad-expr 'bind))
         (return (datum->syntax #'monad-expr 'return))
         (zero (datum->syntax #'monad-expr 'zero))
         (plus (datum->syntax #'monad-expr 'plus)))
      (syntax 
       (let* ((current-monad monad-expr)
              (bind (monad-bind current-monad))
              (return (monad-return current-monad))
              (plus (monad-plus current-monad))
              (zero (monad-zero current-monad)))
         body ...)))]))

(define-syntax (with-monad/location stx)
  (syntax-parse 
   stx
   [(with-monad monad-expr:expr lexeme body:expr ...)
    (with-syntax
        ((current-monad (datum->syntax #'lexeme 'current-monad))
         (bind (datum->syntax #'lexeme 'bind))
         (return (datum->syntax #'lexeme 'return))
         (zero (datum->syntax #'lexeme 'zero))
         (plus (datum->syntax #'lexeme 'plus)))
      (syntax 
       (let* ((current-monad monad-expr)
              (bind (monad-bind current-monad))
              (return (monad-return current-monad))
              (plus (monad-plus current-monad))
              (zero (monad-zero current-monad)))
         body ...)))]))

(define-syntax (mlet* stx)
  (define-syntax-class binder
    (pattern (pattern:expr value:expr)))
  (define-syntax-class simple-binder 
    (pattern (pattern:expr (~datum is:) value:expr)))
  (define-syntax-class specified-binder
    (pattern (pattern:expr (~datum in:) monad:expr value:expr)))
  (define-syntax-class general-binder
    (pattern binder:binder)
    (pattern binder:simple-binder)
    )
  (syntax-parse
   stx
   [(mlet* (~datum in:) monad:expr 
           [binder:binder general-binder:general-binder ...]
           body:expr ...)
    #'(let ((monad-id monad))
        (with-monad/location
         monad-id binder
         ((monad-bind monad-id) binder.value
                                (lambda (id)
                                  (match id
                                    [binder.pattern 
                                     (mlet* in: monad-id [general-binder ...] body ...)])))))]
   [(mlet* (~datum in:) monad:expr
           [simple-binder:simple-binder general-binder:general-binder ...]
           body:expr ...)
    #'(match simple-binder.value
        [simple-binder.pattern
         (mlet* in: monad [general-binder ...]
                body ...)])]
   [(mlet* (~datum in:) monad:expr [] body:expr ...)
    #'(with-monad/location monad monad body ...)]
   [(mlet* [general-binder:general-binder ...] body:expr ...)
    #'(mlet* in: current-monad [general-binder ...] body ...)]))



(define (list-bind v f) 
  (let loop ([acc '()]
             [v v])
    (match v
      [(list) acc]
      [(cons el rest)
       (match (f el)
         [(? list? items) (loop (append acc items)
                                rest)]
         [x (loop (append acc) (list x))])]
      [o (loop acc
               (list v))])))

(define (list-return x) (list x))

(define list-plus append)

(define list-zero '())

(define the-list-monad (monad list-bind list-return list-plus list-zero))

(define the-identity-monad (monad bind return plus zero))

(struct state-doublet (proper state) #:transparent)
(struct state-error (message last-state) #:transparent)
(struct state-fail () #:transparent)

(define (state-return item)
  (lambda (state) (state-doublet item state)))

(define (state-promote thing)
  (match thing
    [(? procedure?) thing]
    [(? state-doublet? doublet) (lambda (state) doublet)]
    [value (lambda (state) (state-doublet value state))]))

(define (state-promote-producer thing)
  (match thing
    [(? procedure?) thing]
    [(? state-doublet? doublet) (lambda (_) (lambda (state) doublet))]
    [value (lambda (_) (state-return value))]))

(define (state-bind state-funish state-fun<proper>ish)
  (let ((state-fun (state-promote state-funish))
        (state-fun<proper> (state-promote-producer state-fun<proper>ish)))
    (lambda (state)
      (match state 
        [(? state-error?) state]
        [(? state-fail?) state]
        [state
         (match (state-fun state)
           [(state-doublet proper new-state)
            ((state-fun<proper> proper) new-state)]
           [(? state-error? error-val)
            error-val]
           [(? state-fail? fail)
            fail]
           [value 
            ((state-fun<proper> value) state)])]
        ))))

(define (state-plus stish1 stish2)
  (let ((stf1 (state-promote stish1))
        (stf2 (state-promote stish2)))
    (lambda (state)
      (match (stf1 state)
        [(state-doublet proper state)
         (stf2 state)]
        [value (stf2 state)]))))

(define state-zero (lambda (state) (state-error "State error.")))

(define the-state-monad (monad state-bind state-return state-plus #f))

(define-syntax (static-lift stx) 
  (define (positive-integer? n)
    (and (integer? n)
         (> n 0)))
  (define (gen-ids n seed-syntax . acc)
    (match acc
      [(list) (gen-ids n seed-syntax '())]
      [(list acc)
       (match n
         [0 (reverse acc)]
         [(? positive-integer?)
          (gen-ids (- n 1) 
                   seed-syntax 
                   (cons (datum->syntax seed-syntax (gensym "lift-id-")) acc))])]))
  (define (ids->pairs-syntax ids)
    (datum->syntax (car ids) 
                   (map
                    (lambda (x)
                      (datum->syntax x (list x x)))
                    ids)))
  (syntax-parse 
   stx
   [(static-lift n-args:number f:expr (~datum into:) monad:expr)
    (let ((ids (gen-ids (syntax->datum #'n-args) #'f)))
      (with-syntax
          (((pair ...) (ids->pairs-syntax ids))
           ((id ...) (datum->syntax (car ids) ids)))
        #'(let ((monad-id monad))
            (lambda (id ...) (mlet* in: monad-id (pair ...) ((monad-return monad-id) (f id ...)))))))]
   [(static-lift n-args:number f)
    (with-syntax 
        ((current-monad (datum->syntax #'f 'current-monad)))
      #'(static-lift n-args f:expr into: current-monad))]))

(define (lift1 f monad) (static-lift 1 f into: monad))

(define (lift2 f monad) (static-lift 2 f into: monad))

(define (lift3 f monad) (static-lift 3 f into: monad))

(define (lift4 f monad) (static-lift 4 f into: monad))

(define (lift5 f monad) (static-lift 5 f into: monad))

(define (lift6 f monad) (static-lift 6 f into: monad))

(define (lift7 f monad) (static-lift 7 f into: monad))

(define (lift8 f monad) (static-lift 8 f into: monad))

(define (lift9 f monad) (static-lift 9 f into: monad))

(define (lift10 f monad) (static-lift 10 f into: monad))

(define (lift11 f monad) (static-lift 11 f into: monad))

(define (lift12 f monad) (static-lift 12 f into: monad))

(define (lift13 f monad) (static-lift 13 f into: monad))

(define (lift14 f monad) (static-lift 14 f into: monad))

(define (lift15 f monad) (static-lift 15 f into: monad))

(define (lift16 f monad) (static-lift 16 f into: monad))

(define (lift17 f monad) (static-lift 17 f into: monad))

(define (lift18 f monad) (static-lift 18 f into: monad))

(define (lift19 f monad) (static-lift 19 f into: monad))

(define (lift20 f monad) (static-lift 20 f into: monad))


(define lift #f)
(let ((lift-table (make-vector 20)))
  (begin (vector-set! lift-table 0 lift1)
         (vector-set! lift-table 1 lift2)
         (vector-set! lift-table 2 lift3)
         (vector-set! lift-table 3 lift4)
         (vector-set! lift-table 4 lift5)
         (vector-set! lift-table 5 lift6)
         (vector-set! lift-table 6 lift7)
         (vector-set! lift-table 7 lift8)
         (vector-set! lift-table 8 lift9)
         (vector-set! lift-table 9 lift10)
         (vector-set! lift-table 10 lift11)
         (vector-set! lift-table 11 lift12)
         (vector-set! lift-table 12 lift13)
         (vector-set! lift-table 13 lift14)
         (vector-set! lift-table 14 lift15)
         (vector-set! lift-table 15 lift16)
         (vector-set! lift-table 16 lift17)
         (vector-set! lift-table 17 lift18)
         (vector-set! lift-table 18 lift19)
         (vector-set! lift-table 19 lift20))
  (set! lift (lambda (n f monad) ((vector-ref lift-table (- n 1)) f monad))))


(define (the-syntax-monad<stx> source-syntax)
  (let ((syntax-return
         (lambda (item) (datum->syntax source-syntax item)))
        (syntax-bind 
         (lambda (stx stx<datum>)
           (let ((datum (syntax->datum stx)))
             (stx<datum> datum)))))
    (monad syntax-bind syntax-return #f #f)))

(define (syntax-return item . where)
  (match where
    [(list) (datum->syntax item #'x)]
    [(list where) (datum->syntax item where)]))

(define (syntax-bind stx stx<p>)
  (let ((p (syntax->datum stx)))
    (stx<p> p)))

(define the-syntax-monad (monad syntax-bind syntax-return #f #f))

(provide mlet* with-monad the-identity-monad the-list-monad the-state-monad state-doublet
         monad monad? monad-bind monad-return monad-plus monad-zero
         list-bind list-return state-bind state-return 
         state-plus state-zero list-plus list-zero
         state-error state-error? state-error-message state-error-last-state state-fail state-fail?
         static-lift the-syntax-monad<stx> lift the-syntax-monad 
         lift1 
         lift2 
         lift3 
         lift4 
         lift5 
         lift6 
         lift7 
         lift8 
         lift9 
         lift10 
         lift11 
         lift12 
         lift13 
         lift14 
         lift15 
         lift16 
         lift17 
         lift18 
         lift19 
         lift20)



