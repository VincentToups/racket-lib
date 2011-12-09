#lang racket

(define (right-ring-over elements)
  (lambda (item) 
    (let loop ((right-elements elements))
      (match right-elements
         [(list) (error (format "Element ~a is not in the ring ~a." item elements))]
         [(list el)
          (if (equal? el item)
              (first elements)
              (error (format "Element ~a is not in the ring ~a." item elements)))]
         [(cons current (cons next rest))
          (if (equal? current item)
               next
               (loop (cons next rest)))]))))

(define (left-ring-over elements)
  (right-ring-over (reverse elements)))

(define (ring-over elements)
  (let ((rt (right-ring-over elements))
        (lt (left-ring-over elements)))
  (lambda (element direction)
    (match direction
      ['right (rt element)]
      ['left (lt element)]))))

(provide left-ring-over right-ring-over ring-over)
