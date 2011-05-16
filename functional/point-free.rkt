#lang racket

(define (>partial f . pargs)
  (lambda args
    (apply f (append pargs args))))

(define (partial< f . pargs)
  (lambda args
    (apply f (append args pargs))))

(define (dec-all f with)
  (lambda args
    (apply f (map with args))))

(define (comp2 f g)
  (lambda args (f (apply g args))))

(define (find lst pred)
  (let loop ((inds '())
             (lst lst)
             (i 0))
    (cond ((empty? lst) (reverse inds)) 
          ((pred (car lst))
           (loop (cons i inds) (cdr lst) (+ i 1)))
          (else
           (loop inds (cdr lst) (+ i 1))))))

(define (zip . args)
  (apply map list args))

(define (replace-inds lst inds vals)
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

(define (any-by lst pred)
  (foldl 
   (lambda (it ac)
     (or ac (pred it)))
   (pred (car lst))
   (cdr lst)))

(define (partial_ sigil f . partial-args)
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
  (let ((fs (reverse fs)))
    (foldl
     (lambda (f o)
       (comp2 f o))
     (car fs)
     (cdr fs))))

(define (f-join f1 f2 j)
  (lambda args
    (j (f1 args) (f2 args))))

(define (f-and f1 f2)
  (lambda args
    (and (f1 args) (f2 args))))

(define (f-or f1 f2)
  (lambda args
    (or (f1 args) (f2 args))))

(provide >partial partial< dec-all 
         partial_
         compose
         f-join
         f-and
         f-or)