#lang racket

(require pure-lands/utilities/dict-state-monad
         pure-lands/utilities/partial-application
         pure-lands/chapter-01/agrilex/parameters)

(define (+/false amount)
  (lambda (old-amount)
    (if old-amount (+ old-amount amount) amount)))

(define get-stamina (get 'stamina))
(define get-field (get 'field))
(define get-letters (get 'letters))
(define get-turn (get 'turn))
(define get-items (get 'items))

(define (set-stamina to)
  (set 'stamina to))
(define (set-field to)
  (set 'field to))
(define (set-letters to)
  (set 'leters to))
(define (set-turn to)
  (set 'turn to))
(define (set-items to)
  (set 'items to))

(define (can? what)
  (match what
	[(? number? n)
	 (build
	  (s <- get-stamina)
	  (==> (<= n s)))]
	[(? stamina-draining-action? s)
	 (can? (- (stamina-cost s)))]))

(define (equal-to-one-of? thing whats)
  (match whats 
	[(list) #f]
	[(cons (? (partial< equal? thing)) rest)
	 #t]
    [(cons _ rest)
     (equal-to-one-of? thing rest)]))

(define (adjust-letter letter amount)
  (if (letter? letter)
      (transform* 'letters letter (+/false amount))
      (error "Can't adjust a non-letter amount.")))

(define (adjust-stamina by)
  (match by
    [(? number? n) (transform* 'stamina (partial< + n))]
    [(? symbol? s) (transform* 'stamina (partial< + (stamina-cost s)))]))

(define (turn+ . args)
  (match args
    [(list) (transform 'turn (partial< + 1))]
    [(list n) (transform 'turn (partial< + n))]))

(define (cons-to what field)
  (transform field (>partial cons what)))

(define (cons-to* . args)
  (let ((what (car args))
        (field (cdr args)))
    (cons-to what field)))

(define (add-item what . args)
  (if (item? what)
      (match args
        [(list) (add-item what 1)]
        [(list n) (transform* 'items what (+/false n))])
      (error (format "Can't add a non-item (~a) to the item list." what))))

(define (remove-item what . args)
  (if (item? what)
      (match args
        [(list)
         (remove-item what 1 'no-error)]
        [(list (? number? n))
         (remove-item what n 'no-error)]
        [(list (? number? n) 'no-error)
         (transform*
          'items what
          (lambda (x)
            (match x
              [#f #f]
              [0   0]
              [(? number? n-held)
               (if (< n-held n) 0
                   (- n-held n))])))]
        [(list (? number? n) 'error)
         (transform*
          'items what
          (lambda (x)
            (match x
              [#f (error (format "Can't remove item ~a because the player doesn't have any." what))]
              [0  (error (format "Can't remove item ~a because the player doesn't have any." what))]
              [(? number? n-held)
               (if (< n-held n) (error (format "Can't remove ~a of item ~a because the player has only ~a." n what n-held))
                   (- n-held n))])))])
      (error (format "Can't add a non-item (~a) to the item list." what))))


(provide
 get-stamina
 get-field
 get-letters
 get-items
 get-field
 set-stamina
 set-field
 set-letters
 set-items
 set-field
 adjust-letter
 adjust-stamina
 turn+
 cons-to
 cons-to*
 add-item
 get-turn
 set-turn
 add-item
 remove-item
 can?)
 

    

