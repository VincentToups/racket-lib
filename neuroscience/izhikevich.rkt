#lang racket

(struct reset: (to))


         
         
(define (make-v-prime 
         #:c1 [c1 100] 
         #:c2 [c2 0.7] 
         #:v1 [v1  60]
         #:v2 [v2  40])
         (lambda (v u i)
           (if (> v 35) (reset: -50)
               (/ (+ i (* c2 (+ v v1) (+ v v2)) (- u)) c1))))

(define (make-u-prime 
         #:c1 [c1 .03]
         #:c2 [c2 -2]
         #:v1 [v1 60])
         (lambda (v u i)
           (if (> v 35) (reset: (+ u 100))
               (* c1 (- (* c2 (+ v v1)) u)))))

(define (make-s-prime 
         #:e [e -70]
         #:n [n +inf.0]
         #:g [g .001]
         #:ds [ds .001]
         #:tc [tc 11])
  (lambda (v inputs g)
      (if (= inputs 0)
          (* g (- e v))
          (reset: (+ g (* inputs ds))))))
         
         