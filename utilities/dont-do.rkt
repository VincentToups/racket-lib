#lang racket

(define-syntax (dont-do stx)
  (syntax-case stx ()
    [(dont-do)
     (syntax (begin))]
    [(dont-do x)
     (syntax (begin))]
    [(dont-do x ...)
     (syntax (begin))]))

(provide dont-do)