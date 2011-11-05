#lang racket

(require utilities/proletariat
         (for-syntax syntax/parse)
         (rename-in roguelikes/white-whale-data3 [floor dungeon-floor] [exit dungeon-exit])
         functional/better-monads
         utilities/rmatch-let
         functional/point-free
         roguelikes/grid
         roguelikes/textish-renderer
         roguelikes/minimap-renderer)

(struct doublet (a b) #:transparent)

(define (none-true list . args)
  (match args
    [(list) 
     (if (empty? list) #t
         (if (car list) #f
             (none-true (cdr list))))]
    [(list f)
     (if (empty? list)
         #t
         (if (f (car list)) #f
             (none-true (cdr list) f)))]))

(define (length=1 list)
  (if (empty? list) #f
      (empty? (cdr list))))

(define (all-true list . args)
  (if (empty? list) (error "all-true undefined for the empty list.")
  (match args
    [(list)
     (if (length=1 list) (car list)
         (if (car list) (all-true (cdr list))
             #f))]
    [(list f)
     (if (length=1 list) (f (car list))
         (if (f (car list)) (all-true (cdr list) f)
             #f))])))

(define (build-return . items)
  (match-lambda 
    [(doublet l g)
     (doublet (map (partial< doublet l) items) g)]))

(define (promote-cmd o)
  (match o
    [(? procedure?) o]
    [value (match-lambda 
             [(doublet l g) (doublet (list (doublet value l)) g)])]))

(define (build-bind cmd cmd<p>)
  (let ((cmd (promote-cmd cmd)))
    (lambda (dblt)
      (match (cmd dblt)
        [(doublet lvals g)
         (let loop ((lvals lvals)
                    (out-lvals '())
                    (g g))
           (match lvals
             [(list) (doublet out-lvals g)]
             [(cons (doublet prop lstate) lvals)
              (match ((cmd<p> prop) (doublet lstate g))
                [(doublet local-lvals g)
                 (loop 
                  lvals
                  (append out-lvals local-lvals)
                  g)])]))]))))

(define build-zero
  (match-lambda 
    [(state-doublet local-state global-state)
     (doublet (list) global-state)]))

(define (lset field . vals)
  (match-lambda 
    [(doublet l g) 
     (doublet
      (map (lambda (val)
             (doublet val (dict-set l field val)))
           vals)
      g)]))

(define (lget field)
  (match-lambda 
    [(doublet l g)
     (doublet (list (doublet (dict-ref l field #f) l))
              g)]))

(define (gget field)
  (match-lambda 
    [(doublet l g)
     (doublet (list (doublet (dict-ref g field) l)) g)]))

(define (gset field val)
  (match-lambda 
    [(doublet l g)
     (doublet (list (doublet val l))
              (dict-set g field val))]))

(define (pset . assignments)
  (match-lambda 
    [(doublet l g)
     (doublet
      (map 
       (lambda (asg)
         (doublet #t
                  (apply dict-set* l asg)))
       assignments)
      g)]))

(define the-build-monad
  (monad build-bind build-return #f build-zero))

(define (build-plus bf1 bf2)
  (mlet* in: the-build-monad 
         ((r1 bf1)
          (r2 bf2))
         (return r2)))

(set! the-build-monad (monad build-bind build-return build-plus build-zero))

(define-syntax (build-do stx)
  (syntax-parse 
   stx
   [(build-do e:expr ...)
    (with-syntax
        ((the-build-monad (datum->syntax #'(e ...) 'the-build-monad)))
      #'(monadic-do in: the-build-monad e ...))]))


(define/class turtle/st (object) 
  'pos '(29 15) 
  'facing 0 
  'helicity 1
  'passable-pred (lambda (item) (if (wall? item) #f
                                    item))
  'movement-fun  (build-return #t))

(define/class build/st (object) 'grid (set-filled-rectangle grid 0 0 60 30 wall))

(define (build-set x y what)
  (mlet* in: the-build-monad 
         ((grid (gget 'grid)))
         (gset 'grid
               (set-at grid (list x y) what))))

(define (jump . args)
  (match args
    [(list (? number? x) (? number? y))
     (lset 'pos (list x y))]
    [(cons (list x y)
           rest)
     (apply lset 'pos (cons (list x y) rest))]))

(define (build-modify x y how)
  (mlet* in: the-build-monad 
         ((grid (gget 'grid))
          (value-at is: (at grid (list x y))))
         (gset 'grid
               (set-at grid (list x y) (how value-at)))))

(define here
  (mlet* in: the-build-monad
         ((pos (lget 'pos))
          (grid (gget 'grid))
          (value-here is: (at grid pos)))
         (return value-here)))

(define (x-of l) (car l))
(define (y-of l) (cadr l))

(define (set-here what)
  (mlet* in: the-build-monad
         ((pos here))
         (build-set (x-of pos) (y-of pos) what)))

(define (modify-here how)
  (mlet* in: the-build-monad
         (((list x y) here))
         (build-modify x y how)))

(define directions (vector 'north 'north-east 'east 'south-east 'south 'south-west 'west 'north-west))
(define dir->vec
  (match-lambda 
    ['north (list 0 1)]
    ['north-east (list 1 1)]
    ['east (list 1 0)]
    ['south-east (list 1 -1)]
    ['south (list 0 -1)]
    ['south-west (list -1 -1)]
    ['west (list -1 0)]
    ['north-west (list -1 1)]))

(define vec->dir 
  (match-lambda 
    [(list 0 1) 'north]
    [(list 1 1) 'north-east]
    [(list 1 0) 'east]
    [(list 1 -1) 'south-east]
    [(list 0 -1) 'south]
    [(list -1 -1) 'south-west]
    [(list -1 0) 'west]
    [(list -1 1) 'north-west]))

(define dir->index
  (match-lambda 
    ['north 0]
    ['north-east 1]
    ['east 2]
    ['south-east 3]
    ['south 4]
    ['south-west 5]
    ['west 6]
    ['north-west 7]
    [(? number? i) (modulo i (length directions))]
    [(? list? x) (dir->index (vec->dir x))]))

(define (index->dir i)
  (vector-ref directions (modulo i (length directions))))

(define (index->vec i)
  (dir->vec (index->dir i)))

(define (extract-grid build-state)
  (match build-state
    [(doublet _ g)
     (dict-ref g 'grid)]))

(define (face . args)
  (match args
    [(list) (lget 'facing)]
    [(? list?) 
     (apply lset 'facing (map dir->index args))]))

(define facing-vector
  (mlet* in: the-build-monad
         ((f (face)))
         (return (index->vec))))

(define (helicity . args)
  (match args
    [(list) (lget 'helicity)]
    [(? list?) 
     (apply lset 'helicity args)]))

(define (positive-integer? n)
  (and (number? n) (>  n 1)))

(define (turn90 n)
  (let ((n (modulo n 4)))
    (match n
      [0 (face)]
      [1 
       (mlet* in: the-build-monad
              ((f (face))
               (h (helicity))
               (i  is: (dir->index f))
               (i+ is: (modulo (+ i (* helicity 2)) (length directions))))
              (face i+))]
      [(? positive-integer?)
       (mlet* in: the-build-monad
              ((<< (turn90 1)))
              (turn90 (- n 1)))])))

(define (turn45 n)
  (let ((n (modulo n 4)))
    (match n
      [0 (face)]
      [1 
       (mlet* in: the-build-monad
              ((f (face))
               (h (helicity))
               (i  is: (dir->index f))
               (i+ is: (modulo (+ i (* helicity 1)) (length directions))))
              (face i+))]
      [(? positive-integer?)
       (mlet* in: the-build-monad
              ((<< (turn90 1)))
              (turn45 (- n 1)))])))

(define turtle-pos 
  (lget 'pos))

(define turtle-position turtle-pos)

(define (scry . args)
  (match args
    [(list x y)
     (mlet* in: the-build-monad
            ((g (gget 'grid)))
            (return (at grid (list x y))))]
    [(list (list x y))
     (scry x y)]
    [(list)
     (mlet* in: the-build-monad
            ((p turtle-pos))
            (scry p))]))


(define (peek . args)
  (match args
    [(list xo yo) 
     (mlet* in: the-build-monad
            ((p turtle-pos))
            (scry (map + p (list xo yo))))]
    [(list (list xo yo))
     (peek xo yo)]
    [(list)
     (mlet* in: the-build-monad
            ((f facing-vector))
            (peek facing-vector))]))

(define (neighboring-positions . args)
  (match args
    [(list x y)
     (mlet* in: the-list-monad
            ((q '(0 1 -1))
             (r '(1 0 -1)))
            (if (and (= 0 q) (= 0 r)) zero
                (return (list (+ x q) (+ y r)))))]
    [(list (list x y))
     (neighboring-positions x y)]))

(define neighbors 
  (mlet* in: the-build-monad
         ((p turtle-pos)
          (ns is: (neighboring-positions p))
          (g (gget 'grid)))
         (return 
          (map (>partial at g) ns))))

(define (set-in-grid . args)
  (match args
    [(list (list x y) what)
     (mlet* in: the-build-monad
            ((g (gget 'grid))
             (_ (gset 'grid (set-at g (list x y) what))))
            (return (list x y)))]
    [(list x y what)
     (set-in-grid (list x y) what)]
    [(list what)
     (mlet* in: the-build-monad
            ((p turtle-position))
            (set-in-grid p what))]))

(define (dip-in-grid . args)
  (match args
    [(list (list x y) how)
     (mlet* in: the-build-monad
            ((g (gget 'grid)))
            (gset 'grid (dip-at g (list x y) how)))]
    [(list x y how)
     (dip-in-grid (list x y) how)]
    [(list how)
     (mlet* in: the-build-monad
            ((p turtle-position))
            (dip-in-grid p how))]))

(define (carve . args)
  (match args
    [(list x y)
     (mlet* in: the-build-monad
            ((h (scry x y)))
            (if (not (floor? h))
                (set-in-grid (list x y) dungeon-floor)
                (return #f)))]
    [(list (list x y))
     (carve x y)]
    [(list)
     (mlet* in: the-build-monad
            ((h turtle-position))
            (begin (carve h)))]))

(define (scry-points points . args)
  (match args
    [(list acc)
     

(define (carve-for-room . args)
  (match args 
    [(list x y)
     (build-do 
      (h <- (scry x y))
      (surround <- 
    

(define (scry/predicate pred . args)
  (mlet* in: the-build-monad
         ((what (apply scry args)))
         (return (pred what))))

(define scry/wall? (>partial scry/predicate wall?))
(define scry/floor? (>partial scry/predicate floor?))

(define (scry/dividing-wall? . args)
  (match args
    [(list x y)
     (monadic-do 
      in: the-build-monad
      (what <- (scry x y))
      (any-floors? <- (foldlm
                       the-build-monad
                       (lambda (pos acc)
                         (monadic-do 
                          in: the-build-monad
                          (nwhat <- (scry pos))
                          (return (or (floor? nwhat) acc))))
                       #f
                       (neighboring-positions (list x y))))
      (return (and (wall? what) any-floors?)))]
    [(list (list x y))
     (scry/dividing-wall? x y)]))



(define scry/floor-or-dividing-wall? 
  (orm in: the-build-monad 
       scry/dividing-wall? 
       scry/floor?))

(define (scry/roomable? . args)
  (build-do
   (v <- (apply scry/floor-or-dividing-wall? args))
   (return (not v))))

(define (all-points-roomable? points)
  (match points
    [(cons hd (list))
     (scry/roomable? hd)]
    [(cons hd tl)
     (build-do
      (r <- (scry/roomable? hd))
      (if r (all-points-roomable? tl)
          (return #f)))]))

(define (set-seed . args)
  (match args
    [(list z w) (lset 'random-state (list z w))]
    [(list (list z w)) (lset 'random-state (list z w))]
    [(list) (lset 'random-state (list 0 0))]))

(define (mod32 x) (modulo x 4294967295))

(define (next-random-int)
  (monadic-do in: the-build-monad
              (rs <- (lget 'random-state))
              ((list z w) 
               is:
               (if (list? rs) rs
                   (list 1 1)))
              (new-z is: (mod32 (arithmetic-shift (+ (* 36969 (bitwise-and z 65535)) (arithmetic-shift z -16)) 16)))
              (new-w is: (mod32 (+ (* 18000 (bitwise-and w 65535)) (arithmetic-shift w -16))))
              (lset 'random-state (list new-z new-w))
              (return (mod32 (+ new-z new-w)))))

(define (next-random-uniform)
  (monadic-do in: the-build-monad
              (randint <- (next-random-int))
              (return (* (+ randint 1) 2.328306435454494e-10))))

(define (next-random-guassian)
  (monadic-do 
   in: the-build-monad
   (u1 <- (next-random-uniform))
   (u2 <- (next-random-uniform))
   (r is: (sqrt (* -2 (log u1))))
   (theta is: (* pi 2 u2))
   (return (* r (cos theta)))))

(define floor/exact (compose inexact->exact floor))
(define ceiling/exact (compose inexact->exact ceiling))

(define (inclusive-range mi mx)
  (let loop 
    ([acc '()]
     [mi mi])
    (cond
      ((= mi mx) (reverse (cons mi acc)))
      ((< mi mx) (loop (cons mi acc) (+ mi 1))))))

(define (carve-room . args)
  (match args
    [(list (list x y) w h)
     (let* ((x-min (- x (floor/exact (/ w 2))))
           (y-min (- y (floor/exact (/ h 2))))
           (x-max (+ x (ceiling/exact (/ w 2))))
           (y-max (+ y (ceiling/exact (/ h 2))))
           (points 
            (monadic-do 
             in: the-list-monad 
             (x <- (inclusive-range x-min x-max))
             (y <- (inclusive-range y-min y-max))
             (return (list x y)))))
       (build-do
        (mapm the-build-monad carve points)))]))

(define (fast-carve-room . args)
  (match args
    [(list (list x y) w h)
     (let* ((x-min (- x (floor/exact (/ w 2))))
           (y-min (- y (floor/exact (/ h 2))))
           (x-max (+ x (ceiling/exact (/ w 2))))
           (y-max (+ y (ceiling/exact (/ h 2)))))
     (build-do
      (g <- (gget 'grid))
      (gset 'grid
            (set-filled-rectangle 
             g
             x-min y-min
             x-max y-max 
             dungeon-floor))))]))

(define (points-between . args)
  (match args
    [(list x1 y1 x2 y2)
     (match-let 
         ([(list x1 x2) (sort (list x1 x2) <)]
          [(list y1 y2) (sort (list y1 y2) <)])
       (monadic-do 
        in: the-list-monad
        (x <- (inclusive-range x1 x2))
        (y <- (inclusive-range y1 y2))
        (return (list x y))))]
    [(list (list x1 y1)
           (list x2 y2))
     (points-between x1 y1 x2 y2)]))

(define (center->boundaries c w h)
  (match-let* 
      ([(list x y) c]
       [x1 (inexact->exact (floor (- x (/ w 2))))]
       [x2 (inexact->exact (ceiling (+ x (/ w 2))))]
       [y1 (inexact->exact (floor (- y (/ h 2))))]
       [y2 (inexact->exact (ceiling (+ y (/ h 2))))])
    (list
     (list x1 y1)
     (list x2 y2))))

(define (room-points c w h)
  (apply points-between (center->boundaries c w h)))
       
(define (no-points-dividing-walls-or-floors-in? points)
  (match points
    [(list) (build-return #t)]
    [(cons hd tl)
     (build-do
      (r <- (scry/dividing-wall? hd))
      (if r (return #f)
          (no-points-dividing-walls-or-floors-in? tl)))]))

(define (can-carve-here? . args)
  (match args
    [(list w h)
     (build-do 
      ((list x y) <- turtle-position)
      (can-carve-here? x y w h))]
    [(list x y w h)
     (let ((points (room-points (list x y) w h)))
       (no-points-dividing-walls-or-floors-in? points))]))

(define (run-build buildf . args)
  (match args
    [(list) (run-build buildf initial-build-state)]
    [(list (? doublet? ibs))
     (buildf ibs)]
    [(cons (? procedure? f) fs)
     ((apply compose (cons f fs)) (run-build buildf initial-build-state))]
    [(cons (? doublet? ibs) fs)
     ((compose fs) (run-build buildf ibs))]))

(define (build-fetch-grid build/st)
  ((compose (partial< at 'grid) doublet-b) build/st))

(define build-fetch-grid->pict (compose to-pict build-fetch-grid))
(define build-fetch-grid->mmap (compose minimap-of build-fetch-grid))

(define-syntax (build/minimap stx)
  (syntax-parse 
   stx
   [(build/test e:expr ...)
    (with-syntax ((the-build-monad (datum->syntax #'(e ...) 'the-build-monad)))
      #'(run-build
       (monadic-do 
        in: the-build-monad
        e ...) build-fetch-grid->mmap))]))

(define-syntax (build stx)
  (syntax-parse 
   stx
   [(build/test e:expr ...)
    (with-syntax ((the-build-monad (datum->syntax #'(e ...) 'the-build-monad)))
      #'(run-build 
       (monadic-do 
        in: the-build-monad
        e ...) build-fetch-grid))]))

(define-syntax (build/vals stx)
  (syntax-parse
   stx
   [(build/vals e:expr ...)
    (with-syntax ((the-build-monad (datum->syntax #'(e ...) 'the-build-monad)))
      #'(run-build 
         (monadic-do 
          in: the-build-monad
          e ...) 
         (lambda (b)
           (map doublet-a (doublet-a b)))))]))
   



(define initial-build-state (doublet turtle/st build/st))



