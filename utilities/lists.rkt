#lang racket

(define (nth n lst)
  (cond
   ((empty? lst) #f)
   ((= n 0)
    (car lst))
   (else
    (nth (- n 1) (cdr lst)))))

(define (sub-nth n val lst)
  (let loop ((n n)
             (lst lst)
             (past '()))
    (cond
     ((empty? lst) (reverse past))
     ((= n 0) (append (reverse past) (cons val (cdr lst))))
     (else
      (loop (- n 1) (cdr lst) (cons (car lst) past))))))

(define (trans-nth n f lst)
  (let loop ((n n)
             (lst lst)
             (past '()))
    (cond
     ((empty? lst) (reverse past))
     ((= n 0) (append (reverse past) (cons (f (car lst)) (cdr lst))))
     (else
      (loop (- n 1) (cdr lst) (cons (car lst) past))))))

(define (split-after-liberally lst n)
  (let loop 
      ((n n)
       (lst lst)
       (acc-head (list)))
    (cond ((= n 0)
           (list (reverse acc-head) lst))
          ((empty? lst)
           (list (reverse acc-head) lst))
          (#t (loop (- n 1)
                    (cdr lst)
                    (cons (car lst) acc-head))))))

(define (bunch-by lst n)
  (let loop ((lst lst)
             (acc (list)))
    (if (empty? lst) 
        (reverse acc)
        (let* ((parts (split-after-liberally lst n))
               (head (car parts))
               (tail (cadr parts)))
          (loop tail (cons head acc))))))

(define (all-by lst pred)
  (if (foldl
       (lambda (it ac)
             (and ac (pred it)))
           (pred (car lst))
           (cdr lst)) #t #f))

(define (identity x) x)

(define (all lst)
  (all-by lst identity))

(define (any-by lst pred)
  (if (foldl 
       (lambda (it ac)
         (or ac (pred it)))
       (pred (car lst))
       (cdr lst)) #t #f))

(define (any lst)
  (any-by lst identity))

(define (none-by lst pred)
  (if (foldl 
       (lambda (it ac)
         (and ac (not (pred it))))
       (not (car lst))
       (cdr lst)) #t #f))

(define (none lst)
  (none-by lst identity))

(define (cons-n n el lst)
  #|
    proc cons-n n el lst conses el n times onto lst
    |#
  (let loop ((i n) (out lst))
    (if (= i 0) out
        (loop (- i 1) (cons el out)))))


(define (rep-list n lst)
  #|
    proc rep-list n lst
      returns a list of lst repeated n times
    |#
  (if (= 0 n) '()
      (reverse (let recur ((rst lst) (out '()))
                 (if (null? rst) out
                     (recur (cdr rst) (cons-n n (car rst) out)))))))

(define (reduce f lst)
  (if (empty? lst) '()
      (foldl 
       f
       (car lst)
       (cdr lst))))

(define (foldl-accumulate f init lst)
  (reverse (foldl
   (lambda (it ac)
     (let ((last (car ac)))
       (cons (f it last) ac)))
   (list init)
   lst)))

(define (reduce-accumulate f lst)
  (foldl-accumulate f (car lst) (cdr lst)))
  
(define (sum lst) (reduce + lst)) 
(define (cum-sum lst) (reduce-accumulate + lst))

(define len length)

(define (range . args)
  (case (length args)
    ((0) '())
    ((1) (range 0 (car args) 1))
    ((2) (range (car args) (cadr args) 1))
    ((3)
     (let loop ((acc (list (car args))))
       (cond 
         ((> (car acc) (- (cadr args) 1)) (reverse (cdr acc)))
         ((= (car acc) (- (cadr args) 1)) (reverse acc))
         (else (loop (cons (+ (caddr args) (car acc)) acc))))))))

(define (list-zip . args)
  (apply map list args))

(define (drop-last lst)
  (reverse (cdr (reverse lst))))

(define (alist . args)
  (let ((pairs (bunch-by args 2)))
    (map 
     (lambda (pair)
       (cons (car pair) (cadr pair)))
     pairs)))

(provide nth sub-nth split-after-liberally 
         bunch-by trans-nth all all-by any 
         any-by rep-list cons-n reduce sum 
         export len none none-by range
         foldl-accumulate reduce-accumulate
         cum-sum list-zip drop-last alist)

