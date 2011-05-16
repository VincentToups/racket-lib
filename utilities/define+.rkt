#lang racket

(define-syntax (define+ stx)
  (syntax-case stx () 
    [(define (name args ...) body ...)
     (syntax (begin 
       (define (name args ...) body ...)
       (provide name)))]
    [(define name val)
     (syntax (begin
       (define name val)
       (provide name)))]))

(define+ (stub x) (+ x 1)) 

(provide define+)