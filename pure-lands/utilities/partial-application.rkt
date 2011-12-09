#lang racket

(define (partial< f . fixed-args)
  (lambda unfixed-args
    (apply f (append unfixed-args fixed-args))))

(define (>partial f . fixed-args)
  (lambda unfixed-args
    (apply f (append fixed-args unfixed-args))))

(define (flat->pairs pairs)
  (let loop ((pairs pairs)
             (acc '()))
    (match pairs
      [(list) (reverse acc)]
      [(cons a (cons b rest))
       (loop rest
             (cons (cons a b) acc))])))

(define (>partial< f . pairs)
  (let ((pairs (flat->pairs pairs)))
    (lambda unfixed-args
      (apply f 
             (let loop ((n 0)
                        (pairs pairs)
                        (args '())
                        (unfixed-args unfixed-args))
               (if (empty? unfixed-args) (reverse args)
                   (match pairs
                     [(list) (append (reverse args) unfixed-args)]
                 [(cons (cons ix val) rest)
                  (if (= ix n)
                      (loop (+ n 1)
                            rest
                            (cons val args)
                            unfixed-args)
                      (loop (+ n 1)
                            pairs
                            (cons (car unfixed-args) args)
                            (cdr unfixed-args)))])))))))

(define-syntax partial
  (syntax-rules (:> <: :><:)
    [(partial f) f]
    [(partial f :> val expr ...)
     (let ((g (>partial f val)))
       (partial g expr ...))]
    [(partial f <: val expr ...)
     (let ((g (partial< f val)))
       (partial g expr ...))]
    [(partial f :><: ix val expr ...)
     (let ((g (>partial< ix val)))
       (partial g expr ...))]))
     
(provide partial partial< >partial >partial<)