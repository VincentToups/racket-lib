#lang racket

;;; A set of utilities for point free programming.  For the most part,
;;; they create new functions by modifying how input arguments are
;;; treated or by partially applying them away.

(define (>partial f . pargs)
  ;;; Partially apply the args pargs on the left of the argument list of f.
  (lambda args
    (apply f (append pargs args))))

(define (partial< f . pargs)
  ;;; Partially apply the args pargs on the right of the argument list of f.
  (lambda args
    (apply f (append args pargs))))

(define (dec-all f with)
  ;;; Decorate all of the arguments of f with the function with.  That
  ;;; is, before the arguments of the new function are passed through
  ;;; to f, each is transformed with with.
  (lambda args
    (apply f (map with args))))

(define (comp2 f g)
  ;;; Function composition.  Return a new function h which is (f (apply g args)).
  (lambda args (f (apply g args))))

(define (find lst pred)
  ;;; Return a list of zero-based indexes indicating which elements of
  ;;; lst are true under pred.
  (let loop ((inds '())
             (lst lst)
             (i 0))
    (cond ((empty? lst) (reverse inds)) 
          ((pred (car lst))
           (loop (cons i inds) (cdr lst) (+ i 1)))
          (else
           (loop inds (cdr lst) (+ i 1))))))

(define (zip . args)
  ;;; Take a all the lists in args and return a single list of lists of elements.
  ;;; (list a b c ...) (list q r s ...) -> (list (list a q) (list b r) ...)
  (apply map list args))

(define (replace-inds lst inds vals)
  ;;; Return a new list which has the same elements as lst, except at
  ;;; inds, where vals are placed instead.
  (let* ((sorted (sort (zip inds vals)
                       (lambda (a b)
                         (< (car a) (car b)))))
         (inds (map car sorted))
         (vals (map cadr sorted)))
    (let loop ((i 0)
               (past '())
               (lst lst)
               (inds inds)
               (vals vals))
      (cond
        ((or (empty? lst) 
             (empty? inds))
         (append (reverse past) lst))
        (else
         (cond 
           ((= i (car inds))
            (loop (+ i 1)
                  (cons (car vals) past)
                  (cdr lst)
                  (cdr inds)
                  (cdr vals)))
           (else
            (loop (+ i 1)
                  (cons (car lst) past)
                  (cdr lst)
                  inds
                  vals))))))))

(define (replace-inds-presorted lst inds vals)
  ;;; As replace-inds, except inds is assumed to be sorted.
  (let loop ((i 0)
             (past '())
             (lst lst)
             (inds inds)
             (vals vals))
    (cond
      ((or (empty? lst) 
           (empty? inds))
       (append (reverse past) lst))
      (else
       (cond 
         ((= i (car inds))
          (loop (+ i 1)
                (cons (car vals) past)
                (cdr lst)
                (cdr inds)
                (cdr vals)))
         (else
          (loop (+ i 1)
                (cons (car lst) past)
                (cdr lst)
                inds
                vals)))))))

(define (trans-nth n f lst)
  ;;; Return a new list based on lst where the nth element is replaced
  ;;; with the result of applying f to that element.
  (let loop ((n n)
             (lst lst)
             (past '()))
    (cond
     ((empty? lst) (reverse past))
     ((= n 0) (append (reverse past) (cons (f (car lst)) (cdr lst))))
     (else
      (loop (- n 1) (cdr lst) (cons (car lst) past))))))

(define (dec-nth f n tr)
  ;;; Return a new function where the nth argument is transformed with
  ;;; tr before being passed to f.
  (lambda args
    (apply f (trans-nth n tr args))))

(define (any-by lst pred)
  ;;; Returns #t when any element of lst is true by pred.
  (foldl 
   (lambda (it ac)
     (or ac (pred it)))
   (pred (car lst))
   (cdr lst)))

(define (partial_ sigil f . partial-args)
  ;;; Position based partial application.  Return a new function g
  ;;; created by fixing arguments of f specified in partial-args.
  ;;;
  ;;; If a sigil is manually provided, positions in partial-args
  ;;; occupied with that sigil are left unapplied in the new function.
  ;;; By default, the sigil is the symbol '_.
  ;;;
  ;;; usage:
  ;;; (define <10 (partial_ < '_ 10))
  ;;; (define >10 (partial_ < 10 '_))
  (if (procedure? sigil) (apply partial_ '_ sigil partial-args)
      (lambda args
        (apply f 
               (let loop ((args args)
                          (pargs partial-args)
                          (acc '()))
                 (cond
                   ((empty? args)
                    (let ((final-args (append (reverse acc) pargs)))
                      (if (any-by final-args (partial< eq? sigil))
                          (error "Unfilled partial application.")
                          final-args)))
                   ((empty? pargs)
                    (append (reverse acc) args))
                   ((eq? (car pargs) sigil)
                    (loop (cdr args) 
                          (cdr pargs)
                          (cons (car args) acc)))
                   (else
                    (loop args
                          (cdr pargs)
                          (cons (car pargs) acc)))))))))

(define (compose . fs)
  ;;; Function composition - compose all of the functions in fs, where
  ;;; the right-most f is applied first.
  (let ((fs (reverse fs)))
    (foldl
     (lambda (f o)
       (comp2 f o))
     (car fs)
     (cdr fs))))

(define (f-join f1 f2 j)
  ;;; Produce a new function from f1 and f2 which applies f1 and f2 to
  ;;; the same arguments and then returns the result of j on the two
  ;;; results.
  (lambda args
    (j (f1 args) (f2 args))))

(define (f-and f1 f2)
  ;;; Return a new function which ands the results of applying f1 and
  ;;; f2 to the input arguments.
  (lambda args
    (and (f1 args) (f2 args))))

(define (f-or f1 f2)
  ;;; Return a new function which ands the results of applying f1 or
  ;;; f2 to the input arguments.
  (lambda args
    (or (f1 args) (f2 args))))

(define (f-not f)
  (lambda args (not (apply f args))))

(define (f-map f)
  ;;; Return a new function which maps f over a list.
  (>partial map f))

(provide >partial partial< dec-all 
         partial_
         compose
         f-join
         f-and
         dec-nth
         f-not
         f-map
         f-or)