#lang racket

(define (escape-quotes string)
  (regexp-replace* #rx"\"" string "\\\\\""))

(define (emacs-pre sexp)
  (format "emacsclient -e \"~a\"" (escape-quotes (format "~s" sexp))))

(define (emacs-eval sexp)
  (system (emacs-pre sexp)))

(define (insert-into-emacs-buffer buffer text)
  (emacs-eval `(with-current-buffer (get-buffer ,buffer) (insert ,text))))

(provide emacs-eval)
