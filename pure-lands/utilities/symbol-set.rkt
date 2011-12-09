#lang racket

(define-for-syntax (add-qmark s)
  (string->symbol
   (string-append (symbol->string s) "?")))

(define (all-symbols? lst)
  (match lst
    [(list x)
     (symbol? x)]
    [(cons x rest)
     (if (symbol? x)
         (all-symbols? rest)
         #f)]))

(define (symbol-in-symbol-set s set)
  (match set
    [(list) #f]
    [(cons hd tl)
     (if (equal? hd s)
         #t
         (symbol-in-symbol-set s tl))]))

(define (unique-cons s set)
  (let loop ((passed '())
             (subset set))
    (match subset
      [(list) (reverse (cons s passed))]
      [(cons hd tl)
       (if
        (equal? hd s) set
        (loop (cons hd passed)
              tl))])))

(define (list->set items)
  (let loop ((items items)
         (set '()))
        (match items
          [(list) set]
          [(cons hd tl)
           (loop tl
                 (unique-cons hd set))])))

(define-syntax (declare-symbol-set stx)
  (syntax-case stx ()
      [(declare-symbol-set name predicate-name member ...)
       #'(begin
           (unless (all-symbols? '(member ...))
             (error "Cannot declare a symbol set on non-symbol members."))
           (define name (list->set '(member ...)))
           (define predicate-name
             (let ((name name))
               (lambda
                   (x)
                 (symbol-in-symbol-set
                  x
                  name)))))]))

(provide declare-symbol-set symbol-in-symbol-set?)
