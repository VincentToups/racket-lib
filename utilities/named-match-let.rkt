#lang racket



(define-syntax (named-match-let stx)
  (syntax-case stx ()
    [(_ name ((match-syntax expr) ...) body ...)
     (let* ((pairs 
             (syntax->datum (syntax ((match-syntax expr) ...))))
            (let-pairs
             (map
              (lambda (pair)
                (list (gensym) (cadr pair))) pairs))
            (match-let-pairs
             (map 
              (lambda (pair let-pair)
                (list (car pair) (car let-pair)))
              pairs
              let-pairs)))
       (with-syntax
           (((new-pairs ...) (datum->syntax (syntax ((match-syntax expr) ...)) let-pairs))
            ((inner-pairs ...) (datum->syntax (syntax ((match-syntax expr) ...)) match-let-pairs)))
         (syntax (let name (new-pairs ...)
                   (match-let (inner-pairs ...)
                     body ...)))))]))


(named-match-let loop ([(list a) (list 0)])
                   (if (< a 10) (loop (list (+ a 1))) a))