#lang racket
(require utilities/proletariat
         roguelikes/white-whale-data3)

(define-multimethod (character o) :: (class-name o))

(define-method (character o) :: melee-weapon "?")

(define-syntax character-of 
  (syntax-rules ()
    [(_ symbol body ...)
     (define-method (character symbol) :: symbol body ...)]))

(character-of object "x")

(character-of shield "o")
(character-of melee-weapon "/")
(character-of shield "o")
(character-of ammunition "=")
(character-of food "%")
(character-of medicine "+")
(character-of player "@")
(character-of monster "m")
(character-of service-droid "t")
(character-of uncanny-duplicate "@")
(character-of menacing-presence ",")
(character-of naked-singularity "*")
(character-of industrial-robot "T")
(character-of combat-robot "&")
(character-of incisive-philosopher "^")
(character-of wall "#")
(character-of 
 floor 
 (with-slots 
  floor (things entities)
  (cond 
    ((not (empty? entities)) (character (car entities)))
    ((not (empty? things)) (character (car things)))
    (else "."))))

(character-of fluid 
              (with-slots 
               fluid (things entities)
               (cond 
                 ((not (empty? entities)) (character (car entities)))
                 ((not (empty? things)) (character (car things)))
                 (else "~"))))

(provide character)
