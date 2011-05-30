#lang racket

(require utilities/fancy-destructuring)

(define (random-normal . args)
  (dlet* ([((: or '(0 1)) m s) args]
          [u1 (random)]
          [u2 (random)]
          [r1 (sqrt (* -2 (log u1)))])
         (+ m (* s (* r1 (cos (* 2 pi u2)))))))

(provide random-normal)


          
          
          