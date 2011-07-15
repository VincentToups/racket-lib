#lang racket

(require racket/dict
         functional/point-free
         utilities/lists)

(define-syntax begin1 
  (syntax-rules ()
    [(begin1) (begin)]
    [(begin1 x) x]
    [(begin1 x expr ...)
     (let ((id x))
       (begin expr ...)
       id)]))

(define (get-from-pile pile n)
  (match (dict-ref pile n
                   (lambda ()
                     (list)))
    [(list) (make-vector n)]
    [(cons head tail) 
     (begin1 head
             (dict-set! pile tail))]))

(define (get-ith i)
  (lambda (v)
    (vector-ref v i)))

(define (map-vector-into into f . vs)
  (let ((n (reduce max (map length vs))))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (vector-set! into (apply f (map (get-ith i) vs)))
            (loop (+ i 1)))
          into))))

(define (v/ v d #:into [into (make-vector (length v))])
  (map-vector-into into (partial< / d) v))
(define (v+ v d #:into [into (make-vector (length v))])
  (map-vector-into into (partial< + d) v))
(define (v- v d #:into [into (make-vector (length v))])
  (map-vector-into into (partial< - d) v))
(define (v* v d #:into [into (make-vector (length v))])
  (map-vector-into into (partial< * d) v))
  
(define (add-to-pile pile . vs)
  (match vs
    [(list) pile]
    [(cons head tail)
     (dict-set! pile (length head) head)
     (apply add-to-pile tail)]))
            
(define (k1 h f t st #:into [into (make-vector (length st))])
  (v* h (f t st) #:into into))

(define (k2 h f t k1 st #:into [into (make-vector (length st))])
  (v* h (f (+ t (/ h 2))
           (map-vector-into
            into 
            (lambda (y k1)
              (+ y (/ k1 2)))
            st
            k1))))


(define (k3 h f t k2 st #:into [into (make-vector (length st))])
  (v* h (f (+ t (/ h 2))
           (map-vector-into
            into 
            (lambda (y k2)
              (+ y (/ k2 2)))
            st
            k2))))

(define (k4 h f t k3 st #:into [into (make-vector (length st))])
  (v* h (f (+ t h)
           (map-vector-into
            into 
            (lambda (y k2)
              (+ y k3))
            st
            k3))))

(define (rk4 h f t st #:pile [pile (make-hash)])
  (let ((k1 (k1 h f t st #:into (get-from-pile pile (length st))))
        (k2 (k2 h f t k1 st #:into (get-from-pile pile (length st))))
        (k3 (k3 h f t k2 st #:into (get-from-pile pile (length st))))
        (k4 (k4 h f t k3 st #:into (get-from-pile pile (length st)))))
    (begin1 (map-vector-into (get-from-pile pile (length st))
                     (lambda (st k1 k2 k3 k4)
                       (+ st (/ (+ k1 (* 2 k2) (* 2 k3) k4) 6)))
                     st k1 k2 k3 k4)
            (add-to-pile pile k1 k2 k3 k4))))
            