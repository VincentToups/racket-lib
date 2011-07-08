#lang racket

(require racket/match)

(define-syntax (r-match stx)
  (syntax-case 