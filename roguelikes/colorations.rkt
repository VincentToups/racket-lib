#lang racket
(require utilities/proletariat
         (prefix-in gui: racket/gui)
         (only-in racket/draw color%)
         roguelikes/white-whale-data3)

(define-multimethod (coloration o) :: (class-name o))
(define-multimethod (background-coloration o) :: (class-name o))

(define-syntax coloration-of 
  (syntax-rules ()
    [(_ symbol body ...)
     (define-method (coloration symbol) :: symbol body ...)]))

(define-syntax background-coloration-of 
  (syntax-rules ()
    [(_ symbol body ...)
     (define-method (background-coloration symbol) :: symbol body ...)]))

(coloration-of object "white")
(background-coloration-of object "black")

(coloration-of 
 player 
 (match player
   [(hash-table 
     ('health 
      (hash-table 
       ('value val)
       ('max mx) (k v) ...)) (q r) ...)
    (let ((proportion (inexact->exact (* 255 (/ val mx)))))
      (make-object color% (- 255 proportion) 0 proportion))]))

(coloration-of stimulant-hypo "green")
(coloration-of med-kit "red")
(coloration-of nano-machine-dose "blue")

(coloration-of snack "green")
(coloration-of meal "red")
(coloration-of feast "blue")

(coloration-of charge-bearing "yellow")

(coloration-of wall "white")
(coloration-of 
 floor 
 (with-slots floor (things entities)
             (cond 
               ((not (empty? entities)) (coloration (car entities)))
               ((not (empty? things)) (coloration (car things)))
               (else "gray"))))

(define *fluid-face-lightness* 190)
(coloration-of water (make-object color% *fluid-face-lightness* *fluid-face-lightness* 255))
(coloration-of lava (make-object color% 255 *fluid-face-lightness* *fluid-face-lightness*))
(coloration-of poison (make-object color% *fluid-face-lightness* 255 *fluid-face-lightness*))

(background-coloration-of 
 water
 (with-slots water (depth)
             (make-object color% 0 25 (+ 50 (inexact->exact (* 175 (/ depth 7)))))))
             
(background-coloration-of 
 lava
 (with-slots lava (depth)
             (make-object color% (+ 50 (inexact->exact (* 175 (/ depth 7)))) 25 0)))
 
(background-coloration-of 
 poison
 (with-slots poison (depth)
             (make-object color% 0 (+ 50 (inexact->exact (* 175 (/ depth 7)))) 25)))


(provide coloration background-coloration)