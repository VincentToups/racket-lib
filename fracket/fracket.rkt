#lang racket

(define (constantly v) (lambda (t) v))

(define (arr f) (lambda (s)
				  (lambda (t)
					(f s t))))

(define (>>> sfa sfb)
  (lambda (s)
	(sfb (sfa s))))

(define (<<< sfb sfa)
  (lambda (s)
	(sfb (sfa s))))

(define (id sf)
  sf)

(define (constant b)
  (lambda (s)
	(constantly b)))

(define (time sf)
  (lambda (t) t))

(define (first sf)
  (lambda (s)
 
