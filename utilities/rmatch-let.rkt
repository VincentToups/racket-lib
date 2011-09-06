#lang racket

(require racket/match
         (for-syntax syntax/parse)
         (for-syntax racket/match)
         syntax/parse)

(define-syntax (rmatch-let stx)
  (define (positive-integer? n)
    (and (number? n)
         (> n 0)))
  (define (gen-ids seed n)
    (let loop ((ids '())
               (n n))
      (match n
        [0 (reverse ids)]
        [(? positive-integer?) 
         (loop (cons (datum->syntax seed (gensym "match-let/loop-dummy-")) ids) (- n 1))]))) 
  (define-syntax-class binder
    (pattern [pat:expr val:expr]))
  (syntax-parse stx
   [(rmatch-let point:id
                    [binder:binder ...]
                    body:expr ...)
    (let* ((n (length (syntax->datum #'(binder ...))))
           (ids (gen-ids #'point n))
           (pats (map (lambda (s)
                        (car (syntax-e s)))
                      (syntax-e #'(binder ...))))
           (inits (map (lambda (s)
                         (cadr (syntax-e s)))
                       (syntax-e #'(binder ...)))))
      (with-syntax 
          (((arg ...) (datum->syntax #'point ids))
           ((pat ...) (datum->syntax (car pats) pats))
           ((init ...) (datum->syntax (car inits) inits)))
        #'(letrec [(point (lambda (arg ...)
                            (match* (arg ...)
                              [(pat ...) body ...])))]
            (point init ...))))]))

   
(provide rmatch-let)