#lang racket

(require (prefix-in nested-dict- pure-lands/utilities/nested-dicts)
         pure-lands/utilities/state-monad)

(define return state-return)

(define (pair a b) (cons a b))
(define (>> . args)
  (let loop ((associations '())
             (args args))
    (match args
      [(list) (reverse associations)]
      [(cons key (cons val rest))
       (loop (cons (pair key val) associations)
             rest)])))

(define initial-letters
  (>> 'f 2 'o 4 'd 2))

(define initial-state
  (>> 'stamina 6
      'total-turns 0
      'points 0
      'letters initial-letters
      'items '()
      'field '()
      'random-state
      #(1 2 3 4 5 6)))

(define (get location)
  (lambda (state)
    (state-result (nested-dict-get state location) state)))

(define (get* . location)
  (get location))

(define (set location value)
  (lambda (state)
    (state-result (void) (nested-dict-set state location value))))

(define (set* . loc-val)
  (set (reverse (cdr (reverse loc-val))) (car (reverse loc-val))))

(define (transform location function)
  (lambda (state)
    (let ((new-state (nested-dict-transform state location function)))
      (state-result (nested-dict-get new-state location)
                    new-state))))

(define random-uniform
  (build 
   (s <- (get 'random-state))
   (g := (vector->pseudo-random-generator s))
   (n := (random g))
   (set 'random-state 
        (vector->immutable-vector (pseudo-random-generator->vector g)))
   (return n)))

(define (right-ring-over elements)
  (lambda (item) 
    (let loop ((right-elements elements))
      (match right-elements
         [(list) (error (format "Element ~a is not in the ring ~a." item elements))]
         [(list el)
          (if (equal? el item)
              (first elements)
              (error (format "Element ~a is not in the ring ~a." item elements)))]
         [(cons current (cons next rest))
          (if (equal? current item)
               next
               (loop (cons next rest)))]))))

(define (left-ring-over elements)
  (right-ring-over (reverse elements)))

(define next-letter
  (right-ring-over '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(define previous-letter
  (left-ring-over '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(struct command-fail (message recovery) #:transparent)
(struct command-success (message) #:transparent)

(define (try-to-till-a-field plot-name)
  (build
   (doable <- (can? 'till))
   (if (not doable)
       (return (command-fail "You're too tired to till a field.  Get some rest." (void)))
       (build
        (avail? <- (plot-name-available? plot-name))
        (if (not avail?)
            (return (command-fail "There is already a field by that name.  Choose another." (void)))
            (build 
             (add-plot-raw plot-name)
             (stamina- (stamina-cost 'till))))))))
        

(define (mutate-letter from f)
  (if (< f 0.5)
      (previous-letter from)
      (next-letter from)))

(define (transform* . loc-fun)
  (transform (reverse (cdr (reverse loc-fun)))
             (car (reverse loc-fun))))

(define (do-nothing state)
  (state-result (void) state))

(define sleep
  (build
   (stamina <- (get 'stamina))
   (cond
     ((> stamina 10) 
      (build (set 'stamina 10)
             (return 10)))
     ((>= stamina 6) (return stamina))
     ((< stamina 6)
      (build
       (set 'stamina 6)
       (return 6))))))

(define (stamina- n)
  (transform 'stamina (lambda (s) (- s n))))

(define (stamina+ n)
  (transform 'stamina (lambda (s) (+ s n))))

(define (stamina< n)
  (build 
   (s <- (get 'stamina))
   (return (< s n))))

(define initial-plot
  (>> 'wet #f
      'planted #f
      'weeds 0
      'harvest #f
      'time-since-planting #f))

(define (stamina-cost what)
  (match what
    ['till 4]
    ['plant 2]
    ['water 1]))

(define (can? what)
  (build
   (s <- (get 'stamina))
   (return
    (>= s (stamina-cost what)))))

(define (add-plot-raw name)
  (set (list 'field (string->symbol* name)) initial-plot))

(define (plot-name-available? name)
  (build
   (plot <- (get (list 'field (string->symbol* name))))
   (return (not plot))))

(define (increment-location . location)
  (transform location (lambda (x) (+ x 1))))

(define (string->symbol* s)
  (match s
    [(? string?) (string->symbol s)]
    [(? symbol?) s]))

(define (get-time-since-planting plot-name)
  (get* 'field (string->symbol* plot-name) 'time-since-planting))

(define (get-weeds plot-name)
  (get* 'field (string->symbol* plot-name) 'weeds))

(define (notify message)
  (transform 'notifications
             (lambda (n)
               (cons message n))))

(define weed-threshold 5)

(define (crop-maturation-time c)
  (+ 4 (match c
         [(or 'e 'a 'i 'o 'n 'r 't 'l 's 'u) 1]
         [(or 'd 'g) 2]
         [(or 'b 'c 'm 'p) 3]
         [(or 'f 'h 'v 'w 'y) 4]
         [(or 'k) 5]
         [(or 'j 'x) 8]
         [(or 'q 'z) 10])))

(define (crop-time-till-seed c)
  (+ 4 (crop-maturation-time c)))

(define (crop-yield c)
  (+ 3
     (match c
       ['e 12]
       [(or 'a 'i) 9]
       ['o 8]
       [(or 'n 'r 't) 6]
       [(or 'l 's 'u 'd) 4]
       ['g 3]
       [(or 'b 'c 'm 'p 'f 'h 'v 'w 'y) 2]
       [(or 'k 'j 'x 'q 'z) 1])))
  
(define (reset-plot plot-name)
  (let ((string->symbol* plot-name))
    (set* 'field plot-name initial-plot)))

(define (plot-gone-to-seed? plot-name)
  (let ((plot-name (string->symbol* plot-name)))
    (build
     (crop <- (get* 'field plot-name 'planted))
     (ticks <- (get-time-since-planting plot-name))
     (weeds <- (get-weeds plot-name))
     (if (and crop
              (or (> ticks (crop-time-till-seed crop))
                  (> weeds weed-threshold)))
         (build
          (notify (format "The field ~a has gone to seed.  You've lost all crops." plot-name))
          (reset-plot plot-name))
         do-nothing))))

(define (update-harvestables plot-name)
  (let ((plot-name (string->symbol* plot-name)))
    (build
     (crop <- (get* 'field plot-name 'planted))
     (ticks <- (get* 'field plot-name 'time-since-planting))
     (if (and crop (> ticks (crop-maturation-time crop)))
         (build
          (notify (format "Crops are ready for harvest in field ~a." plot-name))
          (set* 'field plot-name 'harvest 
                (pair crop (crop-yield crop))))
         do-nothing))))

(define mutation-rate 0.1)

(define (incr x) (if x (+ x 1) 0))
(define (decr x) (if x (- x 1) 0))
                     
(define (build-crop-yield crop)
  (build
   (n := (crop-yield crop))
   (let-build 
    loop
    ((n n)
     (harvest '()))
    (mutate <- random-uniform)
    (direction <- random-uniform)
    (cond 
      ((= n 0) (return harvest))
      ((< mutate mutation-rate)
       (loop 
        (- n 1)
        (nested-dict-transform 
         harvest 
         (mutate-letter crop direction)
         incr)))
      (#t
       (loop 
        (- n 1)
        (nested-dict-transform 
         harvest
         crop 
         incr)))))))
                   
(define (tick-plot plot-name)
  (let ((plot-name (string->symbol* plot-name)))
    (build
     (planted? <- (get (list 'field 
                             plot-name
                             'planted)))
     (wet? <- (get (list 'field
                         plot-name
                         'wet)))
     (if (and wet? planted?)
         (increment-location 'fields plot-name 'time-since-planting)
         do-nothing)
     (increment-location 'fields plot-name 'weeds)
     (set 'wet #f)
     (update-harvestables)
     (plot-gone-to-seed? plot-name))))
                           