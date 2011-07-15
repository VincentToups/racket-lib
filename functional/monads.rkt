#lang racket

(require utilities/lists
         (for-syntax utilities/lists)
         racket/dict)

(define-syntax let-if
  ;;; (let-if name expr true-branch false-branch) Binds name to the
  ;;; result of expression, and then, if it is also a true value,
  ;;; executes true branch, otherwise executes false-branch, each in
  ;;; the scope where name is bound.
  (syntax-rules ()
    [(let-if name pred true false) 
     (let ((name pred))
       (if pred true false))]))

(define-syntax (mlet* stx)
  ;;; Monadic binding form.  (mlet* monad ((var expr) ...) body ...)
  ;;; _monadically_ bind each expression to the variable var in
  ;;; subsequent binding forms and in (begin body ...).
  ;;;
  ;;; The monad form specifies the bind operation to use, where monad
  ;;; must evaluate to a racket dict with entries for 'bind, 'return,
  ;;; 'plus and 'zero, although only 'bind and 'return must be functions.
  (syntax-case stx ()
    [(mlet* monad ((var exp) ...) body ...)
     (syntax 
      (with-monad monad (mlet-inner monad ((var exp) ...) body ...)))]))

(define-syntax (with-monad stx)
  ;;; Evaluate (begin body ...) in a context where current-monad,
  ;;; m-bind, >>=, m-return, m-plus, and m-zero are defined based on
  ;;; the values in the monad specified by the expression monad.
  (syntax-case stx ()
    [(with-monad monad body ...)
     (with-syntax ((current-monad_ (datum->syntax (syntax monad) 'current-monad))
                   (m-bind_ (datum->syntax (syntax monad) 'm-bind))
                   (>>=_ (datum->syntax (syntax monad) '>>=))
                   (m-return_ (datum->syntax (syntax monad) 'm-return))
                   (m-plus_ (datum->syntax (syntax monad) 'm-plus))
                   (m-zero_ (datum->syntax (syntax monad) 'm-zero)))
       (syntax (let ((current-monad_ monad)
                     (m-bind_ (bind-of monad))
                     (m-return_ (return-of monad))
                     (m-plus_ (plus-of monad))
                     (m-zero_ (zero-of monad)))
                 body ...)))]))

(define-syntax (mlet-inner stx)
  ;;; Helper macro for mlet*.
  (syntax-case stx (non-monadically: if:)
    [(mlet-inner monad ((non-monadically: pairs)) body ...)
     (syntax (let* pairs body ...))]
    [(mlet-inner monad ((name (if: monadic-val true-expr false-expr))) body ...)
     (syntax (mlet-inner monad ((id monadic-val) (name (if id true-expr false-expr))) body ...))]
    [(mlet-inner monad ((var exp)) body ...)
     (syntax ((bind-of monad) exp (lambda (var) 
                                    body ...)))]
    [(mlet-inner monad ((non-monadically: pairs) (var1 exp1) ...) body ...)
     (syntax 
      (let* pairs (mlet-inner monad ((var1 exp1) ...) body ...)))]
    [(mlet-inner monad ((name (if: monadic-val true-expr false-expr)) (var1 exp1) ...) body ...)
     (syntax 
      (mlet-inner monad ((id monadic-val) (name (if id true-expr false-expr)) (var1 exp1) ...) body ...))]
    [(mlet-inner monad ((var0 exp0) (var1 exp1) ...) body ...)
     (syntax ((bind-of monad) 
              exp0 
              (lambda (var0) 
                (mlet-inner monad ((var1 exp1) ...) body ...))))]))

;; (define-syntax (monadically stx)
;;   (syntax-case stx ( <- :- )
;;     [(monadically monad expression ...)

(define (retrieve key alist)
  ;;; Retrieve key from the dict in alist.
  (dict-ref alist key))

(define (>partial f . pargs)
  ;;; partially apply the args pargs to the left of f.
  (lambda args
    (apply f (append pargs args))))

(define (partial< f . pargs)
  ;;; partially apply the args pargs to the right of f.
  (lambda args
    (apply f (append args pargs))))

(define bind-of (>partial retrieve 'bind))
(define return-of (>partial retrieve 'return))
(define zero-of (>partial retrieve 'zero))
(define plus-of (>partial retrieve 'plus))

(define (alist-cons key val alist)
  ;;; Cons the key/val pair onto alist in such a way as to avoid
  ;;; duplicate keys.
  (let loop
      ((alist alist)
       (past (list)))
    (cond ((empty? alist) (cons (cons key val) past))
          ((eq? (car (car alist)) key)
           (append (cons (cons key val) past) (cdr alist)))
          (#t (loop (cdr alist) (cons (car alist) past))))))

(define (alist>> maybe-alist . args)
  ;;; Add the key/val pairs in args to the alist maybe-alist.  If
  ;;; maybe-alist is not a list, create a new alist and then add the
  ;;; pairs, including the value maybe-alist as the first key.
  (if (not (list? maybe-alist)) (apply alist>> (list) maybe-alist args)
      (foldl 
       (lambda (pair alist)
         (alist-cons (car pair) (cadr pair)
                     alist))
       maybe-alist
       (bunch-by args 2))))

(define (fswap2 f)
  ;;; Return a new function which swaps the order of the two arguments
  ;;; of f.
  (lambda (a b)
    (f b a)))

(define
  m-list
  ;;; The list or sequence monad.
  (alist>> 'bind
           (lambda (v f)
             (reduce (fswap2 append) (map f v)))
           'return list
           'zero (list)
           'plus append))

(define
  m-list-interleave
  ;;; The list or sequence monad where bind interleaves elements
  ;;; instead of concatenating the lists.
  (alist>> 'bind 
           (lambda (v f)
             (reduce (fswap2 interleave) (map f v)))
           'return list
           'zero (list)
           'plus interleave))


(define
  m-maybe
  ;;; The maybe monad.  Bind bails if any value is #f.
  (alist>> 'bind (lambda (v f)
                   (if v (f v) #f))
           'return (lambda (x) x)
           'zero #f
           'plus (lambda (a b) (and a b))))

(define (lift-left f monad)
  ;;; Make f a monadic function in its first argument.
  (lambda (v . args)
    (mlet* monad ((v v))
           (m-return (apply f v args)))))

(define (lift-nth n f monad)
  ;;; Make f a monadic function in its nth argument.
  (lambda args
    (mlet* monad ((a (nth n args)))
           (m-return (apply f (sub-nth n a args))))))

(define (lift-nth-plus n f monad)
  ;;; Make f a monadic function in its nth argument, but assume f
  ;;; already returns a monadic value, which needs to be combined with
  ;;; m-plus.
  (lambda args
    (with-monad monad
                (reduce m-plus (mlet* monad ((a (nth n args)))
                                               (m-return (apply f (sub-nth n a args))))))))

(define (lift-n n f monad)
  ;;; lift f into the monad monad by its nth argument.
  (if (= n 0) f
      (let loop ((i 1)
                 (lifted (lift-nth 0 f monad))
                 (left (- n 1)))
        (if (= left 0) lifted
            (loop
             (+ i 1)
             (lift-nth-plus i lifted monad)
             (- left 1))))))

(define-syntax (static-lift stx)
  ;;; Statically lift the function f, with arity n, into the monad
  ;;; monad.
  (syntax-case stx ()
    [(static-lift n f monad)
     (with-syntax (((binder ...) (map 
                               (lambda (i)
                                 (datum->syntax (syntax monad) 
                                                (list (string->symbol 
                                                       (format "monadic-arg-~a" i))
                                                      (string->symbol 
                                                       (format "arg-~a" i)))))
                               (range (syntax->datum (syntax n)))))
                   ((monad-arg ...)
                    (map 
                     (lambda (i)
                       (datum->syntax (syntax monad) 
                                               (string->symbol 
                                                (format "monadic-arg-~a" i))))
                     (range (syntax->datum (syntax n)))))
                   ((arg ...)
                    (map 
                     (lambda (i)
                       (datum->syntax (syntax monad) 
                                               (string->symbol 
                                                (format "arg-~a" i))))
                     (range (syntax->datum (syntax n))))))
       (syntax (lambda (arg ...)
                 (mlet* monad (binder ...)
                      ((return-of monad) (f monad-arg ...))))))]))

(define (add-unique item list pred)
  ;;; cons item onto list if it isn't there already (by pred).
  (let loop ((past '())
         (list list))
    (cond ((empty? list)
           (reverse (cons item past)))
          ((pred item (car list))
           (append (reverse past) list))
          (else
           (loop (cons (car list) past) (cdr list))))))

(define (append-unique it ac pred)
  ;;; Append the two lists together in such a way that only one of
  ;;; each element (by pred) is preserved.
  (foldl
   (lambda (item list)
     (add-unique item list pred))
   ac
   it))    

(define (map-cat-unique f v pred)
  ;;; The set monadic bind operation, needs to be partially applied
  ;;; for pred.  Apply f to the elements in v, and collect the unique
  ;;; output elements.
  (let ((lists (map f v)))
    (reduce 
     (lambda (it ac)
       (append-unique it ac pred))
     lists)))

(define (m-set pred)
  ;;; The set monad.
  (alist>>
   'return (lambda (x) (list x))
   'bind (lambda (v f)
           (map-cat-unique f v pred))
   'plus (lambda (a b) 
           (append-unique a b pred))
   'zero '()))

(define m-state
  ;;; The state monad.  
  (alist>> 
   'return (lambda (x) (lambda (state) (list x state)))
   'bind (lambda (v f)
           (lambda (state)
             (let* ((val/state (v state))
                    (val (car val/state))
                    (new-state (cadr val/state))
                    (new-f (f val)))
               (new-f new-state))))
   'plus (lambda (sf1 sf2)
           (lambda (state)
             (let ((val/state (sf2 state)))
               (sf2 (cadr val/state)))))
   'zero 'unbound))

(define-syntax (define-struct-adjuster stx)
  (syntax-case stx ()
    [(_ struct-id field-id)
     (with-syntax ((name (datum->syntax 
                          (syntax struct-id)
                          (string->symbol (string-append
                                           "adjust-" 
                                           (symbol->string (syntax->datum (syntax struct-id)))
                                           "-"
                                           (symbol->string (syntax->datum (syntax field-id)))))))
                   (monadic-name 
                    (datum->syntax 
                          (syntax struct-id)
                          (string->symbol (string-append
                                           "adjust-" 
                                           (symbol->string (syntax->datum (syntax struct-id)))
                                           "-"
                                           (symbol->string (syntax->datum (syntax field-id))) "^")))))
       (syntax (begin 
                 (define (name struct val) 
                   (struct-copy struct-id struct [field-id val]))
                 (define (monadic-name val)
                   (lambda (state)
                     (list val (name state val)))))))]))

(define-syntax (define-struct-adjusters stx)
  (syntax-case stx ()
    [(_ struct-id field-id)
     (syntax (define-struct-adjuster struct-id field-id))]
    [(_ struct-id field-id0 field-id ...)
     (syntax (begin (define-struct-adjuster struct-id field-id0) 
                    (define-struct-adjusters struct-id field-id ...)))]))

(define-syntax (define-struct-getter stx)
  (syntax-case stx ()
    [(_ struct-id field-id)
     (with-syntax ((name (datum->syntax 
                          (syntax struct-id)
                          (string->symbol
                           (string-append
                            (symbol->string (syntax->datum (syntax struct-id)))
                            "-"
                            (symbol->string (syntax->datum (syntax field-id)))
                            ))))
                   (-name (datum->syntax 
                          (syntax struct-id)
                          (string->symbol
                           (string-append
                            (symbol->string (syntax->datum (syntax struct-id)))
                            "-"
                            (symbol->string (syntax->datum (syntax field-id)))
                            "^")))))
       (syntax (define (-name state)
                 (list (name state) state))))]))

(define-syntax (define-struct-getters stx)
  (syntax-case stx ()
    [(_ struct-id field-id)
     (syntax (define-struct-getter struct-id field-id))]
    [(_ struct-id field-id0 field-id ...)
     (syntax (begin (define-struct-getter struct-id field-id0)
                    (define-struct-getters struct-id field-id ...)))]))

(define-syntax (simple-state-struct stx)
  (syntax-case stx ()
    [(_ struct-id field ...)
     (syntax 
      (begin (struct struct-id (field ...))
             (define-struct-getters struct-id field ...)
             (define-struct-adjusters struct-id field ...)))]))

(define (push x)
  (lambda (state)
    (list x (cons x state))))

(define pop
  (lambda (state)
    (list (car state) (cdr state))))



(define stack+
  (mlet* m-state
         ((a pop)
          (b pop))
         (m-return (+ a b))))

(define (m-syntax where)
  ;;; The syntax monad - might be useful?
  (alist>>
   'return (lambda (d)
             (datum->syntax where d))
   'bind 
   (lambda (syntax-value f)
     (let ((naked-value (syntax->datum syntax-value)))
       (f naked-value)))))

(define-syntax (state-define stx)
  ;;; Utility for defining state-functions.  Defines a function of all
  ;;; paramters after the initial parameter, which returns a function of state.
  ;;; Or defines a state function which returns the value specified.
  (syntax-case stx ()
    [(state-define (id state-name) body ...)
     (syntax (define (id) (lambda (state-name) body ...)))]
    [(state-define (id state-name var) body ...)
     (syntax (define (id var)
               (lambda (state-name) body ...)))]
    [(state-define (id state-name var ...) body ...)
     (syntax (define (id var ...)
               (lambda (state-name) body ...)))]
    [(state-define id val)
     (syntax (define id (lambda (state) (list val state))))]))

(state-define (-push stack x) (cons x stack))
             
(provide mlet*
         with-monad
         m-list
         m-list-interleave
         m-set
         m-state
         m-syntax
         m-maybe
         define-struct-adjusters
         define-struct-getters
         state-define
         simple-state-struct
         lift-n
         lift-left
         lift-nth
         lift-nth-plus static-lift)

