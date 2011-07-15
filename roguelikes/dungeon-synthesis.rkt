#lang racket

(require (except-in racketcon/monadic-turtles move add-line-pts turn )
         racket/match
         (prefix-in lists: utilities/lists)
         (prefix-in pf: functional/point-free)
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
       [#f "X"]
       [#t " "]
       ['() " "]
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
            (hc-append row-image (r (dict-ref d (list row j) #f))))
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
            (string-append row-image (r (dict-ref d (list row j) #f) #:mode 'text)))
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
         (error "Chunk access out of range."))]))



(define directions
  (vector 'north 'north-east 'east 'south-east 
          'south 'south-west 'west 'north-west))

(define (vector-ref-modulo v i)
  (vector-ref (modulo i (length v))))

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
                 (clockwise-from face n))]
     ['counter-clockwise
      (set-local 'facing
                 (counter-clockwise-from face n))])))

(define (turtles->chunk f size)
  (let* ((out (f (make-turtles))))
    (match out
      [(doublet x global)
      (dict-ref global 'chunk)])))



(define test-chunk (chunk 60 30 (make-immutable-hash (list (cons (list 10 10) #t))) simple-tile->pict))

(turtles->chunk (place-thing '/) 'x)




