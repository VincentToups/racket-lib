#lang racket
(require (planet "main.ss" ("murphy" "multimethod.plt" 2 1))
         functional/point-free
         racket/match
         racket/dict)

(define-multimethod (at obj field) :: (vector-immutable (class-name obj) (class-name field)))
(define-method (at obj field) :: #(object symbol)
  (dict-ref obj field))

(define (class-table class . args)
  (let loop [(object (make-immutable-hash (list (cons 'class class))))
             (args args)]
    (match args
      [(list) object]
      [(cons key (cons val rest))
       (loop (dict-set object key val) rest)])))

(define (-> d . keys)
  (let loop ((d d)
             (keys keys))
    (match keys 
      [(list) d]
      [(cons key rest)
       (loop (at d key) rest)])))

(define object (class-table 'object ))
(define (object? thing)
  (derived? (class-name thing) 'object))

(define (dict-fold f-key-val-ac init d)
  (let loop ([ac init]
             [pos (dict-iterate-first d)])
    (if (not pos) ac
        (loop (f-key-val-ac (dict-iterate-key d pos)
                            (dict-iterate-value d pos)
                            ac)
              (dict-iterate-next d pos)))))

(define (dict-merge from into)
  (dict-fold 
   (lambda (key val d)
     (dict-set d key val))
   into
   from))

(define (class-name o)
  (match o
    [(? hash? immutable?) 
     (dict-ref o 'class 'immutable-hash)]
    [(? list?) 'list]
    [(? vector?) 'vector]
    [(? string?) 'string]
    [(? number?) 'number]
    [(? struct?) 'struct]
    [(? symbol?) 'symbol]
    [(? char?) 'char]
    [(? boolean?) 'boolean]
    [(? keyword?) 'keyword]
    [(? pair?) 'pair]
    [(? hash?) 'hash]))

(define derive-from-object (partial< derive 'object))
(for-each derive-from-object
          '(dict list vector string number struct symbol char boolean keyword pair))

(define derives-from derive)
(define derives-from? derived?)

(define (>> class parents . args)
  (let* ((new-fields (apply class-table class args))
         (merge-list (reverse (cons new-fields parents)))
         (class-dict (foldl dict-merge (car merge-list) (cdr merge-list))))
    (for-each (lambda (parent)
                (derive class (class-name parent))) parents)
    class-dict))

(struct depending-on-data (keys lam) #:transparent)

(define-syntax depending-on
  (syntax-rules ()
    [(depending-on (key/arg ...) body ...)
     (depending-on-data '(key/arg ...)
                        (lambda (key/arg ...) body ...))]))

(define (adjust* object . args)
  (match args 
    [(list) object]
    [(cons key (cons val rest))
     (apply adjust (dict-set object key 
                       (if (depending-on-data? val)
                           (apply (depending-on-data-lam val) 
                                  (map 
                                   (>partial at object)
                                   (depending-on-data-keys val)))
                           val)) rest)]))

(define (adjust object . args)
  (let ((original-object object))
    (let loop ((object object)
               (args args))
      (match args 
        [(list) object]
        [(cons key (cons val rest))
         (loop (dict-set object key 
                           (if (depending-on-data? val)
                               (apply (depending-on-data-lam val) 
                                      (map 
                                   (>partial at original-object)
                                   (depending-on-data-keys val)))
                               val)) rest)]))))
      
;(define rock (>> 'rock (list object) 'type 'pyrite))

;(define-syntax define/class 
;  (syntax-rules ()
;    [(define/class name-id (parents ...) field/val ...)
;     (define name-id (>> 'name-id (list parents ...) field/val ...))]))

(define-syntax (define/class stx)
  (define (append-? symb)
    (string->symbol (string-append (symbol->string symb) "?")))
  (syntax-case stx ()
      [(define/class name-id (parents ...) field/val ...)
       (with-syntax ([predicate-name (datum->syntax
                                     #'name-id
                                     (append-? (syntax->datum #'name-id)))])
         (syntax (begin
                   (define name-id (>> 'name-id (list parents ...) field/val ...))
                   (define (predicate-name o)
                     (derived? (class-name o) 'name-id)))))]))

(define (flat->dict lst)
  (let loop ([lst lst]
             [d   (make-immutable-hash '())])
    (match lst
      [(list) d]
      [(cons key (cons val rest))
       (loop rest (dict-set d key val))])))

(define (mix new-class-name from-class into-class . args)
  (let ((resolvers (flat->dict args)))
    (dict-fold
     (lambda (key new into-class)
       (let* ((resolver (dict-ref resolvers key (lambda () (lambda (new old) new))))
              (old (dict-ref into-class key #f)))
         (dict-set into-class key (resolver new old))))
     into-class
     from-class)))

(define-multimethod (as ob class-nm) :: (vector-immutable (class-name ob) class-nm))
(define-method (as o s) :: #(object string)
  (format "~a" o))
(define-method (as o n) :: #(object number)
  (string->number o))
(define-method (as n _) :: #(number number)
  n)

(define-syntax with-a-slot 
  (syntax-rules ()
    [(with-a-slot object (slot symbol) body ...)
     (let ((symbol (at object 'slot)))
       body ...)]
    [(with-a-slot object (slot) body ...)
     (let ((slot (at object 'slot)))
       body ...)]
    [(with-a-slot object slot body ...)
     (let ((slot (at object 'slot)))
       body ...)]))

(define-syntax with-slots 
  (syntax-rules ()
    [(with-slots object () body ...)
       (begin body ...)]
    [(with-slots object (binder binders ...) body ...)
     (let ((id object))
       (with-a-slot id binder 
                    (with-slots id (binders ...) body ...)))]))

(define-match-expander -at
  (syntax-rules ()
    [(-at key pat)
     (app (lambda (id) 
            (at id key)) pat)]))

(define-match-expander obj
  (syntax-rules ()
    [(obj (key pat) ...)
     (and (-at key pat) ...)]))

(provide define/class
         object
         >>
         derives-from
         derives-from?
         class-name
         dict-merge
         dict-fold
         class-table
         mix
         adjust
         adjust*
         depending-on
         ->
         as
         at
         with-slots
         with-a-slot
         obj
         (all-from-out (planet "main.ss" ("murphy" "multimethod.plt" 2 1))))


