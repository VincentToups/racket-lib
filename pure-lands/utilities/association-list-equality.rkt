#lang racket

(struct equal-to-nothing_ () #:transparent)
(define equal-to-nothing (equal-to-nothing_))

(define (asc-equal? a1 a2)
  (let ((k1 (map car a1))
		(k2 (map car a2)))
	(if (not
		 (= (length k1)
			(length k2)))
		#f
		(let loop
			((ks k1))
		  (match ks
			[(list) #t]
			[(cons k ks)
			 (let ((v1 (dict-ref a1 k))
				   (v2 (dict-ref a2 k equal-to-nothing)))
			   (if (equal? v1 v2)
				   (loop ks)
				   #f))])))))

(provide asc-equal?)

