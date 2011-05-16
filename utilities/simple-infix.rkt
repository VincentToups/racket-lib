#lang racket

(define-syntax ($ stx)
  (syntax-case stx ()
    [($ first function rest ...)
     (syntax (function first rest ...))]))

(provide $)