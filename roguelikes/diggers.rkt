#lang racket
(require utilities/fancy-destructuring
         utilities/lists
         utilities/one-of
         utilities/state-machines
         functional/point-free
         functional/monads)

(one-of (10 'north)
        (50 'south)
        (10 'east)
        (10 'west))

         
(define digger-machine
  (alist 'state 'initial
         'pos (cons 32 16)
         'world '()
         'initial (lambda (m)
                    (let ((new-pos (cons (random 64) (random 32)))
                          (initial-room 
                           