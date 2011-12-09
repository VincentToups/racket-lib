#lang racket

(define (pair a b) (cons a b))

(define (transform state key-or-keys function)
  (match key-or-keys
    [(or (? symbol? key) 
         (list key))
     (let ((value (dict-ref state key #f)))
       (dict-set state key (function value)))]
    [(cons key others)
     (let ((value (dict-ref state key '())))
       (dict-set state key
                 (transform value others function)))]))

(define (with state key-or-keys fun)
  (match key-or-keys
    [(or (? symbol? key)
         (list key))
     (fun (dict-ref state key #f))]
    [(cons key rest)
     (with (dict-ref state key '()) rest fun)]))

(define (get state key-or-keys)
  (with state key-or-keys identity))

(define (set state key-or-keys value)
  (transform state key-or-keys (lambda (x) value)))

(define (>> . args)
  (let loop ((associations '())
             (args args))
    (match args
      [(list) (reverse associations)]
      [(cons key (cons val rest))
       (loop (cons (pair key val) associations)
             rest)])))

(define (update>> ndict . args)
  (match args
    [(? (empty? args)) ndict]
    [(cons key (cons val rest))
     (apply update>> (set ndict key val) rest)]))

(provide 
 get set with transform >> update>>)