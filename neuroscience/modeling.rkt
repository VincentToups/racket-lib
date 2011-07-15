#lang racket

(require racket/dict
         racket/match)

(struct struct:component (does does-form doc depends-on)
  #:property
  prop:procedure (struct-field-index does)
  #:property
  prop:custom-write 
  (lambda (comp port mode)
    (display (format "<does: ~s\n" (struct:component-does-form comp)) port)
    (display (format " doc:  ~s>\n" (struct:component-doc  comp)) port)))

(define-syntax (component stx)
  (syntax-case stx ()
    [(component doc (arg1 ...) body ...)
     (with-syntax ((this-component-id (datum->syntax (syntax doc) 'this-component)))
       (syntax
        (letrec ((this-component-id (struct:component
                         (lambda (arg1 ...) body ...)
                         '(lambda (arg1 ...) body ...)
                         doc
                         '(arg1 ...))))
                 this-component-id)))]))

(define (positive-integer? x)
  (and (integer? x)
       (positive? x)))
(define (non-negative-integer? x)
  (and (number? x)
       (or (positive-integer? x)
           (= 0 x))))

(define (first-n lst n) 
  (let loop ([lst lst]
             [acc '()]
             [n n])
    (match n
      [0 (reverse acc)]
      [(? positive-integer?)
       (loop (cdr lst) (cons (car lst) acc)
             (- n 1))])))

(define (drop-n lst n)
  (let loop ([lst lst]
             [n n])
    (match n
      [0 lst]
      [(? positive-integer?) 
       (loop (cdr lst)
             (- n 1))])))

(define (make-fresh-disambiguator)
  (let ((table (make-hash)))
    (lambda (symbol)
      (match (dict-ref table symbol #f)
        [(? non-negative-integer? n) 
         (dict-set! table symbol (+ n 1))
         (string->symbol
          (string-append 
           (symbol->string symbol)
           (number->string (+ n 1))))]
        [#f 
         (dict-set! table symbol 0)
         symbol]))))

(define (disambiguate-symbols list-of)
  (map (make-fresh-disambiguator) list-of))

(define (component+ c1 c2)
  (match-let* ([(struct:component does1 does-form1 doc1 depends-on1) c1]
               [(struct:component does2 does-form2 doc2 depends-on2) c2]
               [doc (format "addition of ~s and ~s" doc1 doc2)]
               [depends-on (disambiguate-symbols (append depends-on1 depends-on2))]
               [form `(lambda ,depends-on
                        (+ (,does-form1 ,@(first-n depends-on (length depends-on1)))
                           (,does-form2 ,@(drop-n depends-on (length depends-on1)))))]
               [arity1 (length depends-on1)]
               [arity2 (length depends-on2)]
               [does (lambda args
                       (+ (does1 (first-n arity1))
                          (does2 (drop-n arity2))))])
    (struct:component does form doc depends-on)))
                       
               
                                                                    
               

