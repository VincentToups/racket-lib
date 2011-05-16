#lang racket 
(require rnrs/hashtables-6)

(define-syntax (let-list stx)
  (syntax-case stx (: :> &)
    [(let-list ((: vars ...)) lst body ...)
     (syntax (let-list (vars ...) (car lst) body ...))]
    [(let-list ((:> forms ...)) lst body ...)
     (syntax (let-table (forms ...) (car lst) body ...))]
    [(let-list (symbol) lst body ...)
     (syntax (let* ((v lst)
                    (symbol (car v)))
               body ...))]
    [(let-list (& symbol) lst body ...)
     (syntax (let* ((v lst)
                    (symbol v))
               body ...))]
    [(let-list ((: vars ...) symbol ...) lst body ...)
     (syntax (let ((v lst))
               (let-list (vars ...) (car v)
                         (let-list (symbol ...) (cdr v) body ...))))]
    [(let-list ((:> forms ...) symbol ...) lst body ...)
     (syntax (let ((v lst))
               (let-table (forms ...) (car v)
                          (let-list (symbol ...) (cdr v) body ...))))]
    [(let-list (symbol0 symbol ...) lst body ...)
     (syntax (let* ((v lst)
                   (symbol0 (car v)))
               (let-list (symbol ...) (cdr v) body ...)))]))

(define-syntax (let-table stx)
  (syntax-case stx (: :>)
    [(let-table (((: vars ...) key)) table body ...)
     (syntax (let* ((v table)
                    (lst (table-get v key)))
               (let-list (vars ...) lst 
               body ...)))]
    [(let-table (((:> forms ...) key)) table body ...)
     (syntax (let* ((v table)
                    (table (table-get v key)))
               (let-table (forms ...) lst 
               body ...)))]
    [(let-table ((symbol key)) table body ...)
     (syntax (let* ((v table)
                    (symbol (table-get v key)))
               body ...))]
    [(let-table (((: vars ...) key0) (symbol key) ...) table body ...)
     (syntax (let* ((v table)
                    (lst (table-get v key0)))
               (let-list (vars ...) lst
                         (let-table ((symbol key) ...) v body ...))))]
    [(let-table (((:> forms ...) key0) (symbol key) ...) table body ...)
     (syntax (let* ((v table)
                    (inner-table (table-get v key0)))
               (let-table (forms ...) inner-table
                         (let-table ((symbol key) ...) v body ...))))]
    [(let-table ((symbol0 key0) (symbol key) ...) table body ...)
     (syntax (let* ((v table)
                    (symbol0 (table-get v key0)))
               (let-table ((symbol key) ...) v body ...)))]))

(define (alist-ref table key)
  (cond ((empty? table) #f))
        ((eq? (car (car table)) key
         (cadr (car table))))
        (#t 
         (alist-ref (cdr table) key)))

(define (table-get table key)
  (cond ((hash? table)
         (hash-ref table key))
        ((list? table)
         (alist-ref table key))))

(define-syntax (dlet* stx)
  (syntax-case stx (: :>)
    [(dlet* (((: vars ...) val)) body ...)
     (syntax (let-list (vars ...) val body ...))]
    [(dlet* (((:> forms ...) val)) body ...)
     (syntax (let-table (forms ...) val body ...))]
    [(dlet* ((var val)) body ...)
     (syntax (let ((var val)) body ...))]
    [(dlet* (((: vars ...) val) rest ...) body ...)
     (syntax (let-list (vars ...) val
                       (dlet* (rest ...) body ...)))]
    [(dlet* (((:> forms ...) val) rest ...) body ...)
     (syntax (let-table (forms ...) val
                        (dlet* (rest ...) body ...)))]
    [(dlet* ((var val) rest ...) body ...)
     (syntax (let ((var val)) 
               (dlet* (rest ...) body ...)))]))

(define-syntax (defn stx)
  (syntax-case stx ()
    [(defn (name args ...) body ...)
     (syntax (define (name . arglist)
               (let-list (args ...) arglist body ...)))]))

(define-syntax (fn stx)
  (syntax-case stx ()
    [(fn (args ...) body ...)
     (syntax (lambda arglist 
               (let-list (args ...) arglist body ...)))]))
                   
(define-syntax (elet stx)
  (syntax-case stx ()
    [(elet (?) body ...)
     (error "Elet requires an even number of binding expressions.")]
    [(elet (var val) body ...)
     (syntax (dlet* ((var val)) body ...))]
    [(elet (var val rest ...) body ...)
     (syntax (dlet* ((var val)) (elet (rest ...) body ...)))]))

(provide let-list let-table dlet* defn fn elet)
