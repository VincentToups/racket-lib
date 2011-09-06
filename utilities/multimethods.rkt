#lang racket

(require racket/dict
         functional/point-free)

(define multimethod-heirarchy (make-parameter (make-hash '())))

(define (dict-dip dict key transformation . otherwise)
  (match otherwise
    [(list) (dict-dip dict key transformation (lambda () (error 'dict-dip "No value associated with ~a in ~a." key dict)))]
    [(list otherwise-value)
     (let ((current-value (dict-ref dict key otherwise-value)))
       (dict-set dict key (transformation current-value)))]))

(define (dict-dip! dict key transformation . otherwise)
  (match otherwise
    [(list) (dict-dip! dict key transformation (lambda () (error 'dict-dip "No value associated with ~a in ~a." key dict)))]
    [(list otherwise-value)
     (let ((current-value (dict-ref dict key otherwise-value)))
       (dict-set! dict key (transformation current-value)))]))


(define (dict-dip-cons dict key val)
  (dict-dip dict key (>partial cons val) '()))

(define (dict-dip-add-to-set dict key val)
  (dict-dip dict key (partial< dict-set val #t) (make-immutable-hash '())))

(define (add-parent-relation child parent . heir)
  (match heir
    [(list) 
     (dict-dip! (multimethod-heirarchy) 'parents (lambda (parents)
                            (dict-dip-add-to-set parents parent child))
               (make-immutable-hash '()))]
    [(list alternative-heirarchy)
     (parameterize ([multimethod-heirarchy alternative-heirarchy])
       (add-parent-relation child parent))]))

(define (add-child-relation parent child . heir)
  (match heir 
    [(list)
     (dict-dip! (multimethod-heirarchy) 'children (lambda (children)
                                                   (dict-dip-add-to-set children child parent))
               (make-immutable-hash '()))]
    [(list alternative-heirarchy)
     (parameterize [(multimethod-heirarchy alternative-heirarchy)]
                  (add-child-relation parent child))]))

(define (derive parent child . heir)
  (match heir
    [(list)
     (add-child-relation parent child) 
     (add-parent-relation child parent)]
    [(list alt-heir)
     (parameterize [(multimethod-heirarchy alt-heir)]
       (derive parent child))]))

(define (hash-keys/empty o)
  (if (hash? o) (hash-keys o) '()))

(define (parents-of o . heir)
  (match heir
    [(list)
     (hash-keys/empty (dict-ref (dict-ref (multimethod-heirarchy) 'parents '()) o '()))]
    [(list h)
     (parameterize [(multimethod-heirarchy h)]
       (parents-of o))]))

(define (any-by lst pred)
  (match lst
    [(list) #f]
    [(cons first rest)
     (if (pred first) #t
         (any-by rest pred))]))

(define (reduce-or-empty f lst)
  (match lst
    [(list) (list)]
    [(? list?)
     (foldl
      f
      (car lst)
      (cdr lst))]))
      

(define (isa_ o1 o2)
  (if (equal? o1 o2) 0
      (let loop ((cost 1)
                 (parents (parents-of o1)))
        (cond 
          ((any-by parents (partial< equal? o2))
           cost)
          ((empty? parents) #f)
          (else
           (loop (+ cost 1)
                 (reduce-or-empty append (map parents-of parents))))))))

(struct multi-isa [first rest])
(define (list->

(define (isa? m1 m2)
  (match m1
    [(cons first rest)
     