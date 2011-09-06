#lang racket

(require (except-in racketcon/monadic-turtles move add-line-pts turn )
         racket/match
         (prefix-in lists: utilities/lists)
         (prefix-in pf: functional/point-free)
         (prefix-in m:  functional/monads)
         (prefix-in pg: utilities/planar-geometry)
         slideshow)

(struct chunk (width height dict tile-renderer)
  #:property
  prop:custom-write 
  (lambda (c port mode)
    (match mode 
      [#f (display (format (chunk->text c)) port)]
      [#t (display (format (chunk->text c)) port)]
      [0  (write c port)]
      [1  (write c port)])))

(struct wall (properties))
(struct floor (properties things entities))
(struct fluid (properties things entities depth))
(struct staircase (properties direction))
(struct weapon (properties type attack))
(struct shield (properties type defense))
(struct food (properties type amount))
(struct scroll (properties type))
(struct medicine (properties type amount))

(define (thing->text thing)
  (match thing
    [(weapon properties type attack)
     "/"]
    [(shield properties type defense)
     "O"]
    [(food properties type amount)
     "="]
    [(scroll properties type)
     "}"]
    [(medicine properties type)
     "+"]))

(define (location->text l)
  (match l
    [(wall p) "#"]
    [(floor p things e)
     (if (empty? things) " "
         (thing->text (car things)))]
    [(fluid p t e d) "~"]
    [(staircase p d) 
     (match d
       ['up ">"]
       ['down "<"])]))

(define (location->pict thing)
  

(define (chunk-at c . args)
  (match args
    [(list i j)
     (match c
       [(chunk w h d r)
        (if (and (< i w)
                 (< j h))
            (dict-ref d (list i j) #f)
            'out-of-bounds)])]
    [(list (pg:point i j))
           (chunk-at c i j)]))

(define (simple-tile->pict item #:mode [mode 'pict])
  (match mode 
    ['pict
     (match item 
       [#f (filled-rectangle 8 8)]
       [#t (rectangle 8 8)]
       ['() (rectangle 8 8)]
       [(cons x rest) (rectangle 8 8)])]
    ['text
     (match item
       [(list) " "]
       [#f "X"]
       [#t " "]
       [(cons '/ rest) "/"]
       [(cons 'o rest) "o"]
       [(cons '\{ rest) "{"]
       [(cons '< rest) "<"]
       [(cons '> rest) ">"]
       [(cons '@ rest) "@"])]))

(define (chunk-row->pict c row)
  (match c
    [(chunk w h d r)
     (if (>= row h) (error "Tried to render a chunk-row out of range.")
         (foldl
          (lambda (j row-image)
            (hc-append row-image (r (dict-ref d (list j row) #f))))
          (blank 0 0)
          (lists:range w)))]))

(define (chunk->pict c)
  (match c
    [(chunk w h d r)
     (foldl (lambda 
                (it ac)
              (vc-append ac (chunk-row->pict c it)))
            (blank 0 0)
            (lists:range h))]))

(define (chunk-row->text c row)
  (match c
    [(chunk w h d r)
     (if (>= row h) (error "Tried to render a chunk-row out of range.")
         (foldl
          (lambda (j row-image)
            (string-append row-image (r (dict-ref d (list j row) #f) #:mode 'text)))
          ""
          (lists:range w)))]))

(define (chunk->text c)
  (match c
    [(chunk w h d r)
     (foldl (lambda 
                (it ac)
              (string-append ac (chunk-row->text c it) "\n"))
            "\n"
            (lists:range h))]))


(define (dip-location c i j f)
  (match c
    [(chunk w h d r)
     (if (and (< i w) 
              (< j h))
         (let* ((at (dict-ref d (list i j) '()))
                (at (if at at '())))
           (chunk w h (dict-set d (list i j) (f at)) r))
         (error "dip-location" "chunk access at ~a out of range (~a)" (list i j) (list 'range: w h)))]))



(define directions
  (vector 'north 'north-east 'east 'south-east 
          'south 'south-west 'west 'north-west))

(define (vector-ref-modulo v i)
  (vector-ref v (modulo i (vector-length v))))

(define (dir->vector dir)
  (match dir
    ['north (pg:point 0 1)]
    ['north-east (pg:point 1 1)]
    ['east (pg:point 1 0)]
    ['south-east (pg:point 1 -1)]
    ['south (pg:point 0 -1)]
    ['south-west (pg:point -1 -1)]
    ['west (pg:point -1 0)]
    ['north-west (pg:point -1 1)]))

(define (vector->dir vector)
  (match vector
    [(pg:point 0 1) 'north]
    [(pg:point 1 1) 'north-east]
    [(pg:point 1 0) 'east]
    [(pg:point 1 -1) 'south-east]
    [(pg:point 0 -1) 'south]
    [(pg:point -1 -1) 'south-west]
    [(pg:point -1 0)  'west]
    [(pg:point -1 1) 'north-west]))

(define (find v pred)
  (let ((n (vector-length v)))
    (let loop ([inds '()]
               [i 0])
      (cond 
        ((< i n)
         (if (pred (vector-ref v i))
             (loop (cons i inds)
                   (+ i 1))
             (loop inds
                   (+ i 1))))
        (else (reverse inds))))))

(define find-first (pf:compose car find))

(define (find-first-equal-to v item)
  (find-first v (lambda (c) (equal? c item))))

(define (clockwise-from dir)
  (match dir
    [(? symbol?) 
     (vector-ref-modulo directions (+ (find-first-equal-to directions dir) 1))]
    [(? pg:point?)
     (clockwise-from (vector->dir dir))]))

(define (counter-clockwise-from dir)
  (match dir
    [(? symbol?) 
     (vector-ref-modulo directions (- (find-first-equal-to directions dir) 1))]
    [(? pg:point?)
     (clockwise-from (vector->dir dir))]))

(define (positive-integer? n)
  (and (integer? n)
       (positive? n)))

(define (counter-clockwise-from-n start n)
  (match (modulo n 8)
    [0 start]
    [(? positive-integer? n)
     (counter-clockwise-from-n (clockwise-from start) (- n 1))]))

(define (clockwise-from-n start n)
  (match (modulo n 8)
    [0 start]
    [(? positive-integer? n)
     (clockwise-from-n (clockwise-from start) (- n 1))]))



(define (face dir)
  (set-local 'facing 
             (match dir
               ['north (pg:point 0 1)]
               ['north-east (pg:point 1 1)]
               ['east (pg:point 1 0)]
               ['south-east (pg:point 1 -1)]
               ['south (pg:point 0 -1)]
               ['south-west (pg:point -1 -1)]
               ['west (pg:point -1 0)]
               ['north-west (pg:point -1 1)])))

(define (turn-direction-conjugate helicity how)
  (match (list helicity how)
    [(list 'clockwise 'up) 'clockwise]
    [(list 'clockwise 'down) 'counter-clockwise]
    [(list 'counter-clockwise 'up) 'counter-clockwise]
    [(list 'counter-clockwise 'down) 'clockwise]))

(define fresh-chunk (chunk 60 30 (make-immutable-hash '()) simple-tile->pict))

(define (make-turtles)
  (doublet 
   (list
    (cons 'pos (pg:point 30 15))
    (cons 'facing 'north)
    (cons 'helicity 'clockwise))
   (list
    (cons 'chunk fresh-chunk))))

(define (carve)
  (turtles-let* 
   ((pos (get-local 'pos (pg:point 0 0)))
    (chunk (get-global 'chunk fresh-chunk)))
   (set-global 'chunk
               (dip-location chunk 
                             (pg:point-x pos)
                             (pg:point-y pos)
                             (lambda (v)
                               (if v v
                                   '()))))))

(define (carve-point pt)
  (turtles-let*
   ((chunk (get-global 'chunk)))
   (set-global
    'chunk 
    (dip-location chunk
                  (pg:point-x pt)
                  (pg:point-y pt)
                  (lambda (v)
                    (if v v
                        '()))))))

(define (carve-points lst)
  (match lst
    [(list) #f]
    [(cons pt 
           (list))
     (carve-point pt)]
    [(cons pt pts)
     (turtles-let*
      ((_ (carve-point pt)))
      (carve-points pts))]))

(define (carve-rectangle r)
  (match-let* 
    ([(pg:rectangle p1 p2) r]
     [(pg:point x1 y1) p1]
     [(pg:point x2 y2) p2]
     [(list x1 x2) (sort (list x1 x2) <)]
     [(list y1 y2) (sort (list y1 y2) <)])
    (carve-points 
     (m:mlet* m:m-list 
             ((x (lists:range x1 (+ 1 x2)))
              (y (lists:range y1 (+ 1 y2))))
             (list (pg:point x y))))))
  
     
(define (place-thing thing)
  (turtles-let* 
   ((pos (get-local 'pos (pg:point 0 0)))
    (chunk (get-global 'chunk fresh-chunk)))
   (set-global 'chunk 
               (dip-location chunk
                             (pg:point-x pos)
                             (pg:point-y pos)
                             (lambda (v)
                               (match v
                                 [#f (list thing)]
                                 [#t (list thing)]
                                 [(? list?) (cons thing v)]))))))
                                     

(define (turn how n)
  (turtles-let*
   ((he (get-local 'helicity 'clockwise))
    (face (get-local 'facing 'north)))
   (match (turn-direction-conjugate he how)
     ['clockwise 
      (set-local 'facing 
                 (clockwise-from-n face n))]
     ['counter-clockwise
      (set-local 'facing
                 (counter-clockwise-from-n face n))])))

(define (cturn)
  (turn 'up 2))

(define (point-in-chunk? p c)
  (match-let
      ([(pg:point i j) p]
       [(chunk w h d r) c])
    (and (>= i 0)
         (>= j 0)
         (< i w)
         (< j h))))
 
(define (default-step-action p1 p2)
  (carve-point p2))

(define (default-step-fail-action p1 p2)
  (turtles-return p1))


(define (step . args)
  (match args
    [(list) 
     (m:mlet* m-turtles
      ((p (get-local 'pos (pg:point 15 15)))
       (f (get-local 'facing 'north))
       (c (get-global 'chunk))
       (non-monadically:
        ((tentatively (pg:point+ p (dir->vector f))))))
      (if (point-in-chunk? tentatively c)
          (m:mlet* m-turtles
                   ((np (set-local 'pos tentatively))
                    (a  (get-local 'step-action default-step-action)))
                   (a p np))
          (m:mlet* m-turtles
                   ((a (get-local 'step-fail-action default-step-fail-action)))
                   (a p tentatively))))]
    [(list 1) (step)]
    [(list n)
     (turtles-let*
      ((p (step)))
      (step (- n 1)))]))

(define (peak-at . args)
  (match args
    [(list)
     (turtles-let* 
      ((p (get-local 'pos (pg:point 15 15)))
       (f (get-local 'facing 'north)))
      (peak (pg:point+ p (dir->vector f))))]
    [(list pt)
     (turtles-let* 
      ((chunk (get-global 'chunk)))
      (turtles-return 
       (chunk-at chunk pt)))]))

(define (peak . args)
  (match args
    [(list) (peak-at)]
    [(list n)
     (turtles-let* 
      ((p (get-local 'pos))
       (f (get-local 'facing))
       (p (pg:point+ p (pg:point-scale (dir->vector f) n))))
      (peak-at p))]))

(define (turtles->chunk f size)
  (let* ((out (f (make-turtles))))
    (match out
      [(doublet x global)
      (dict-ref global 'chunk)])))





(define test-chunk (chunk 60 30 (make-immutable-hash (list (cons (list 10 10) #t))) simple-tile->pict))






