#lang racket

(require utilities/proletariat
         (rename-in roguelikes/white-whale-data3 [floor dungeon-floor] [exit dungeon-exit])
         functional/better-monads
         utilities/rmatch-let
         functional/point-free)

(define/class grid (object) 'keys '() 'data '() 'min-x +inf.0 'max-x -inf.0 'min-y +inf.0 'max-y -inf.0)

(define-method (at grid pos) :: #(grid list)
  (match grid 
    [(hash-table grid ('data data) (_ _) ...)
     (dict-ref data pos #f)]))

(define-method (at grid s) :: #(grid symbol)
  (match s
    ['count (with-slots grid (data) (dict-count data))]
    [_ (call-next-method)]))
    

(define (pos< p1 p2)
  (match (list p1 p2)
    [(list
      (list (? number? i1)
            (? number? j1))
      (list (? number? i2)
            (? number? j2)))
     (if (= i1 i2) (< j1 j2)
         (< i1 i2))]))

(define (pos-cons p pl . acc)
  (match acc 
    [(list) (pos-cons p pl '())]
    [(list acc)
     (match pl
       [(list) (reverse (cons p acc))]
       [(cons h rest)
        (cond 
          ((pos< p h) (append (reverse (cons p acc)) pl))
          ((equal? p h) (append (reverse acc) pl))
          (else (pos-cons p rest (cons h acc))))])]))

(define (pos-remove p pl . acc)
  (match acc
    [(list) (pos-remove p pl '())]
    [(list acc)
     (match pl
       [(list) (reverse acc)]
       [(cons h rest)
        (cond
          ((equal? p h) (append (reverse acc) rest))
          (else (pos-remove p rest (cons h acc))))])]))

(define (update-grid-min/max grid)
  (let* ((positions (at grid 'keys)))
    (if (empty? positions)
        (adjust grid 'min-x +inf.0 'max-x -inf.0 'min-y +inf.0 'max-y -inf.0)
        (let*
         ((first (car positions))
          (init (list (car first) (cadr first)
                      (car first) (cadr first))))
          (match 
              (foldl 
               (lambda (it ac)
                 (match it
                   [(list minx miny maxx maxy)
                    (match ac
                      [(list cx cy)
                       (list 
              (min minx cx)
              (min miny cy)
              (max maxx cx)
              (max maxy cy))])]))
               init
               (cdr positions))
            [(list minx miny maxx maxy)
             (adjust grid
                     'min-x minx
                     'max-x maxx
                     'min-y miny
               'max-y maxy)])))))
    
    (define-multimethod (set-at gr p val) :: (vector-immutable (class-name gr) (class-name p)))
    (define-method (set-at gr p val) :: #(grid list)
  (adjust gr 'data 
          (depending-on (data) (dict-set data p val))
          'keys (depending-on (keys) (pos-cons p keys))
          'min-x (depending-on (min-x) 
                               (min min-x (car p)))
          'min-y (depending-on (min-y)
                               (min min-y (cadr p)))
          'max-x (depending-on (max-x)
                               (max max-x (car p)))
          'max-y (depending-on (max-y)
                               (max max-y (cadr p)))))

(define-multimethod (unset-at grid pos) :: (vector-immutable (class-name grid) 
                                                             (class-name pos)))
(define-method (unset-at grid pos) :: #(grid list)
  (update-grid-min/max
   (adjust grid 
           'data 
           (depending-on (data)
                         (dict-remove data pos))
           'keys (depending-on (keys)
                               (pos-remove pos keys)))))



(define-multimethod (dip-at gr p dip) :: (vector-immutable (class-name gr) (class-name p)))
(define-method (dip-at gr p dip) :: #(grid list)
  (let ((val (at gr p)))
    (set-at gr p (dip val))))

(define-multimethod (square-extent o) :: (class-name o))
(define-method (square-extent g) :: grid
  (cond 
    ((= 0 (at g 'count)) '())
    (else
     (with-slots g (min-x max-x min-y max-y)
                (list
                 (list min-x min-y)
                 (list max-x max-y))))))

(define pos-i car)
(define pos-j cadr)

(define-match-expander pos 
  (syntax-rules ()
    [(pos x y) (list x y)]))

(define (set-horizontal-line grid x-start x-stop y what)
  (match-let ([(list x-start x-stop) (sort (list x-start x-stop) <)])
    (let loop [(acc grid)
               (i x-start)]
      (if (> i x-stop) acc
          (loop (set-at acc (list i y) what)
                (+ i 1))))))

(define (set-vertical-line grid y-start y-stop x what)
  (match-let ([(list y-start y-stop) (sort (list y-start y-stop) <)])
    (let loop [(acc grid)
               (j y-start)]
      (if (> j y-stop) acc
          (loop (set-at acc (list x j) what)
                (+ j 1))))))

(define (set-rectangle grid . args)
  (match args
    [(list (pos min-x min-y) (pos max-x max-y) thing)
     (set-rectangle grid min-x min-y max-x max-y thing)]
    [(list min-x min-y max-x max-y thing)
     ((compose 
       (partial< set-horizontal-line min-x max-x min-y thing)
       (partial< set-horizontal-line min-x max-x max-y thing)
       (partial< set-vertical-line (+ min-y 1) (- max-y 1) min-x thing)
       (partial< set-vertical-line (+ min-y 1) (- max-y 1) max-x thing))
      grid)]))

(define (set-filled-rectangle grid . args)
  (match args
    [(list (pos min-x min-y) (pos max-x max-y) thing)
     (set-rectangle grid min-x min-y max-x max-y thing)]
    [(list min-x min-y max-x max-y thing)
     (let loop ((x min-x)
                (grid grid))
       (if (> x max-x) grid
           (loop (+ x 1)
                 (set-vertical-line grid min-y max-y x thing))))]))

(define (set-room grid . args)
  (match args
    [(list (pos min-x min-y) (pos max-x max-y))
     (set-rectangle grid min-x min-y max-x max-y)]
    [(list min-x min-y max-x max-y)
     ((compose
       (partial< set-filled-rectangle 
                 (+ min-x 1) (+ min-y 1)
                 (- max-x 1) (- max-y 1)
                 dungeon-floor)
       (partial< set-rectangle 
                 min-x min-y
                 max-x max-y
                 wall))
      grid)]))

(struct doublet (a b) #:transparent)

(define (build-return . items)
  (match-lambda 
    [(doublet l g)
     (doublet (map (partial< doublet l) items) g)]))

(define (build-bind cmd cmd<p>)
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
                g)])]))])))

(define build-zero 
            
(provide grid grid? dip-at unset-at set-at square-extent pos-i pos-j 
         set-rectangle set-vertical-line set-horizontal-line 
         set-filled-rectangle set-room)
