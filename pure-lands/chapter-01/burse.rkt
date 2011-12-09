#lang racket

(require racket/dict)

(define 
  denominations
  '((fifties  . 50)
    (twenties . 20)
    (tens     . 10)
    (fives    . 5)
    (ones     . 1)
    (quarters . 0.25)
    (dimes    . 0.10)
    (nickels  . 0.05)
    (pennies  . 0.01)))

(define
  the-empty-burse
  '((fifties  . 0)
    (twenties . 0)
    (tens     . 0)
    (fives    . 0)
    (ones     . 0)
    (quarters . 0)
    (dimes    . 0)
    (nickels  . 0)
    (pennies  . 0)))

(define (pair-first p)
  (car p))

(define (pair-second p)
  (cdr p))

(define (burse-amount burse denominations)
  (let loop ((amount 0)
             (burse burse))
    (if (empty? burse) amount
        (let* ((denom (pair-first (first burse)))
           (count (pair-second (first burse)))
           (denom-amount (dict-ref denominations denom)))
          (loop (+ amount (* count denom-amount))
                (rest burse))))))
  

(define (increment-count burse denom)
  (let ((n (dict-ref burse denom)))
    (dict-set burse denom (+ n 1))))

(define (make-payment amount denominations initial-burse)
  (display (format "amount ~a~n" amount))
  (cond
    ((or (= 0 amount) (< amount 0.01)) initial-burse)
    ((empty? denominations)
     (error "Insufficient denominations to make exact payment."))
    (#t
     (let ((current-denomination (pair-first (first denominations)))
           (current-amount (pair-second (first denominations))))
       (if (>= amount current-amount) 
           (make-payment
            (- amount current-amount)
            denominations
            (increment-count initial-burse current-denomination))
           (make-payment 
            amount
            (rest denominations)
            initial-burse))))))
    