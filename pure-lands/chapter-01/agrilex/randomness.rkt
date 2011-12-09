#lang racket

(require pure-lands/utilities/dict-state-monad)

(define (initialize-random-state . args)
  (match args 
    [(list) (set 'random-state #(1 2 3 4 5 6))]
    [(list (and (? vector?)
                (? immutable? v)))
     (set 'random-state v)]))

(define random-uniform
  (build 
   (s <- (get 'random-state))
   (g := (vector->pseudo-random-generator s))
   (n := (random g))
   (set 'random-state 
        (vector->immutable-vector (pseudo-random-generator->vector g)))
   (state-return n)))

(provide random-uniform)