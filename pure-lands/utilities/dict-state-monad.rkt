#lang racket

(require (prefix-in nested-dict- pure-lands/utilities/nested-dicts)
         pure-lands/utilities/state-monad)

(define (pair a b) (cons a b))

(define (get location)
  (lambda (state)
    (state-result (nested-dict-get state location) state)))

(define (get* . location)
  (get location))

(define (set location value)
  (lambda (state)
    (state-result (void) (nested-dict-set state location value))))

(define (set* . loc-val)
  (set (reverse (cdr (reverse loc-val))) (car (reverse loc-val))))

(define (transform location function)
  (lambda (state)
    (let ((new-state (nested-dict-transform state location function)))
      (state-result (nested-dict-get new-state location)
                    new-state))))

(define (transform* . loc-fun)
  (transform (reverse (cdr (reverse loc-fun)))
             (car (reverse loc-fun))))

(define (with location fun)
  (build
   (val <- (get location))
   (state-return (fun val))))

(define (with* . loc/fun)
  (let ((location (reverse (cdr (reverse loc/fun))))
        (fun (car (reverse cdr))))
    (with location fun)))

(provide get get* set set* transform transform* with with*
         (all-from-out pure-lands/utilities/state-monad))
 