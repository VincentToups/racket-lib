#lang racket

(define (nth n lst)
  ;;; return the nth value in lst, #f if there is no nth value.
  (cond
   ((empty? lst) #f)
   ((= n 0)
    (car lst))
   (else
    (nth (- n 1) (cdr lst)))))

(define (sub-nth n val lst)
  ;;; Substitute the nth value of lst with val.  If there are fewer
  ;;; than n values, return the unmodified list.
  ;;; Returns a new list, not a copy.
  (let loop ((n n)
             (lst lst)
             (past '()))
    (cond
     ((empty? lst) (reverse past))
     ((= n 0) (append (reverse past) (cons val (cdr lst))))
     (else
      (loop (- n 1) (cdr lst) (cons (car lst) past))))))

(define (trans-nth n f lst)
  ;;; Replace the nth value of list with f on that value.
  ;;; Returns a new list, not a copy.
  (let loop ((n n)
             (lst lst)
             (past '()))
    (cond
     ((empty? lst) (reverse past))
     ((= n 0) (append (reverse past) (cons (f (car lst)) (cdr lst))))
     (else
      (loop (- n 1) (cdr lst) (cons (car lst) past))))))

(define (split-after-liberally lst n)
  ;;; Return a list of two lists, created by splitting lst after the
  ;;; nth element.  "liberally" refers to the fact that if lst has
  ;;; fewer than n elements, the first returned list is just lst and
  ;;; the second is the empty list.
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
  ;;; Return a list produced by bunching elements of lst into
  ;;; sub-lists of length n.  The last list may have fewer than n
  ;;; elements.
  (let loop ((lst lst)
             (acc (list)))
    (if (empty? lst) 
        (reverse acc)
        (let* ((parts (split-after-liberally lst n))
               (head (car parts))
               (tail (cadr parts)))
          (loop tail (cons head acc))))))

(define (all-by lst pred)
  ;;; Returns true of pred is true for all elements in lst.
  (if (foldl
       (lambda (it ac)
         (and ac (pred it)))
       (pred (car lst))
       (cdr lst)) #t #f))

(define (identity x) x) ; the handy identity function.

(define (all lst)
  ;;; Returns true if all elements of lst are true-values under and.
  (all-by lst identity))

(define (any-by lst pred)
  ;;; Returns true if any element in lst is true by pred.
  (if (empty? lst) #f
      (if (foldl 
           (lambda (it ac)
             (or ac (pred it)))
           (pred (car lst))
           (cdr lst)) #t #f)))

(define (any lst)
  ;;; Returns true if any element in lst is true by and.
  (any-by lst identity))

(define (none-by lst pred)
  ;;; Returns true if none of the elements in lst are true by pred.
  (not (any-by lst pred)))
(define (none lst)
  ;;; Returns true if none of the elements in lst are true by and.
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
  ;;; Reduce (left) folds f over (cdr lst) with (car lst) as the initial value.
  (if (empty? lst) '()
      (foldl 
       f
       (car lst)
       (cdr lst))))

(define (foldl-accumulate f init lst)
  ;;; Like fold, except each accumulation is concatenated into a list,
  ;;; which is finally returned.
  (reverse (foldl
            (lambda (it ac)
              (let ((last (car ac)))
                (cons (f it last) ac)))
            (list init)
            lst)))

(define (reduce-accumulate f lst)
  ;;; fold-accumulate f over (cdr lst) with (car lst) as the initial
  ;;; value of the accumulator.
  (foldl-accumulate f (car lst) (cdr lst)))

(define (sum lst) (reduce + lst)) 
(define (cum-sum lst) (reduce-accumulate + lst))

(define len length) ; silly short version of length.

(define (range . args)
  ;;; produce ranges of numbers.
  ;;; (range 3) -> (0 1 2) ; specify max value, from zero, step 1
  ;;; (range 1 3) -> (1 2) ; specify starting value, max value
  ;;; (range 0 5 16) -> (0 5 10 15) ; specify start, stepsize and max.
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
  ;;; Create a new list from the lists in args which is the list of
  ;;; lists containing the consecutive elements of the input lists.
  (apply map list args))

(define (drop-last lst)
  ;;; Return a new list without the last element of lst.
  (reverse (cdr (reverse lst))))

(define (alist . args)
  ;;; Short hand for creating an alist from a flat list of
  ;;; symbol/value pairs.
  (let ((pairs (bunch-by args 2)))
    (map 
     (lambda (pair)
       (cons (car pair) (cadr pair)))
     pairs)))

(define (add-to-front element list)
  ;;; Prepend the element onto the list - same as cons.
  (if (not (list? list)) (error "Second argument to prepend must be a list.")
      (cons element list)))

(define (add-to-back elememt list)
  ;;; Append the element to list, return a new list.
  (if (not (list? list)) (error "Second argument to prepend must be a list.")
      (reverse (cons elememt (reverse list)))))

(define (interleave l1 l2)
  ;;; Interleave the elements in l1 and l2.
  (let loop
      ((l1 l1)
       (l2 l2)
       (ac '()))
    (cond ((empty? l1) (append (reverse ac) l2))
          ((empty? l2) (append (reverse ac) l1))
          (else
           (loop l2 (cdr l1) (cons (car l1) ac))))))

(provide nth sub-nth split-after-liberally 
         bunch-by trans-nth all all-by any 
         any-by rep-list cons-n reduce sum 
         export len none none-by range
         foldl-accumulate reduce-accumulate
         cum-sum list-zip drop-last alist
         interleave
         add-to-back add-to-front)


