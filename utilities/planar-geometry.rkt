#lang racket
(require racket/match
         functional/point-free
         functional/monads
         utilities/lists
         utilities/define+)

(struct point (x y)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display
           (format "(point ~s ~s)"
                   (point-x s)
                   (point-y s))
           port)))

(struct line-segment (p1 p2)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(line-segment " port)
          (display (line-segment-p1 s) port)
          (display " " port)
          (display (line-segment-p2 s) port)
          (display ")" port)))
           
(struct line (m b)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(line ~s ~s)" (line-m s) (line-b s))))

(struct vertical-line (x positive)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display (format "(vertical-line ~s ~s)" (vertical-line s) (vertical-line-positive s)) port)))

(struct circle (c r)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display (format "(circle ") port)
          (display (circle-c s) port)
          (display (circle-r s) port)
          (display ")" port)))

(struct rectangle (p1 p2)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(rectangle " port)
          (display (rectangle-p1 s) port)
          (display " " port)
          (display (rectangle-p2 s) port)
          (display ")" port)))
        
(struct polygon (points)
        #:property prop:custom-write
        (lambda (s port mode)
          (display "(polygon (list " port)
          (let loop ((points (polygon-points s)))
            (cond ((empty? points) (display "))" port))
                  (else
                   (display (car points) port)
                   (loop (cdr points)))))))

(struct domain (start stop
                      kind)
        #:property prop:custom-write
        (lambda (s port mode)
          (display (format "(domain ~s ~s '~s)"
                           (domain-start s)
                           (domain-stop s)
                           (domain-kind s))
                           port)))


(define (domain* a b kind)
  (match (sort (list a b) <)
    [(list a b) (domain a b kind)]))

(define (number-in-domain? n a-domain)
  (match a-domain
    [(domain start stop kind)
     (match kind
       ['exclusive
        (and (> n start)
             (< n stop))]
       ['left-inclusive
        (and (>= n start)
             (< n stop))]
       ['right-inclusive
        (and (> n start)
             (<= n stop))]
       ['inclusive
        (and (>= n start)
             (<= n stop))]
       ['left-exclusive
        (and (> n start)
             (<= n stop))]
       ['right-exclusive
        (and (>= n start)
             (< n stop))])]))

(define 2pi (* 2 pi))
(define π pi)
(define 2π (* 2 pi))

(define (point+ p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (point (+ x1 x2)
                    (+ y1 y2))))

(define (point- p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (point (- x1 x2)
                    (- y1 y2))))

(define (point. p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (+ (* x1 x2) (* y1 y2))))

(define point-mag
  (match-lambda [(point x y)
                 (sqrt (+ (* x x) (* y y)))]))

(define (point-dist p1 p2)
  (point-mag (point- p1 p2)))

(define (point-scale p s)
  (match p
    [(point x y) (point (* x s) (* y s))]))

(define (normalize-point p)
  (let ((m (point-mag p)))
    (point-scale p (/ 1 m))))

(define (point-field-* p1 p2)
  (match-let
   ([(point x1 y1) p1]
    [(point x2 y2) p2])
   (point (- (* x1 x2)
             (* y1 y2))
          (+ (* x1 y2)
             (* y1 x2)))))

(define (point= p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(define (line-segment-vertical? a-line-seg)
  (match a-line-seg
    [(line-segment p1 p2)
     (= (point-x p1) (point-y p2))]))

(define (line-segment-slope a-line-seg)
  (match-let* ([(line-segment p1 p2) a-line-seg]
               [(point x1 y1) p1]
               [(point x2 y2) p2])
              (if (line-segment-vertical? a-line-seg)
                  (cond ((= y1 y2) +nan.f)
                        ((< y1 y2) +inf.f)
                        ((> y1 y2) -inf.f))
                  (/ (- y1 y2)
                     (- x1 x2)))))

(define (line-segment-intercept a-line-seg)
  (match-let* ([(line-segment p1 p2) a-line-seg]
               [(point x1 y1) p1]
               [(point x2 y2) p2])
              (if (line-segment-vertical? a-line-seg)
                  (cond ((= y1 y2) +nan.f)
                        ((< y1 y2) +inf.f)
                        ((> y1 y2) -inf.f))
                  (- y1 (* (line-segment-slope a-line-seg) x1)))))

(define (line-segment->line line-seg)
  (if (line-segment-vertical? line-seg)
      (vertical-line (point-x (line-segment-p1 line-seg))
                     (positive? (line-segment-slope line-seg)))
      (line (line-segment-slope line-seg)
            (line-segment-intercept line-seg))))

(define (line-at a-line x)
  (match a-line
    [(line m b) 
     (+ (* m x) b)]
    [(vertical-line lx p)
     (cond ((= lx x) 0)
           ((and (< x lx) p)
            -inf.f)
           ((and (> x lx) p)
            +inf.f)
           ((and (< x lx) (not p))
            -inf.f)
           ((and (> x lx) (not p))
            +inf.f))]))

(define (line->pair-of-points a-line)
  (match a-line
    [(line m x) (list (point 0 (line-at a-line 0)) (point 1 (line-at a-line 1)))]
    [(vertical-line lx p)
     (match p
       [#t (list (point lx 0) (point lx 1))]
       [#f (list (point lx 0) (point lx -1))])]))

(define (line->line-segment a-line)
  (match a-line
    [(line m x) (line-segment (point 0 (line-at a-line 0)) (point 1 (line-at a-line 1)))]
    [(vertical-line lx p)
     (match p
       [#t (line-segment (point lx 0) (point lx 1))]
       [#f (line-segment (point lx 0) (point lx -1))])]))

(define (line->lambda line)
  (>partial line-at line))

(define points-xs
  (f-map point-x))

(define points-ys
  (f-map point-y))

(define (sort-points-on-xs points)
  (sort points (dec-all < point-x)))

(define (square x) (* x x))

(define (make-line-via-regression points)
  (let* ((points (sort-points-on-xs points))
         (n (length points))
         (xs (points-xs points))
         (ys (points-ys points))
         (sx (reduce + xs))
         (sy (reduce + ys))
         (sx2 (reduce
               (dec-nth + 0 square) xs))
         (sxy (foldl
               (lambda (x y ac)
                 (+ ac (* x y)))
               0
               xs
               ys))
         (d (- (* n sx2) (* sx sx)))
         (intercept
          (/ (- (* sx2 sy) (* sx sxy)) d))
         (slope
          (/ (- (* n sxy) (* sx sy)) d)))
    (line slope intercept)))

(define (make-lambda-via-regression points)
  (line->lambda (make-line-via-regression points)))

(define (polygon-edges a-polygon)
  (match a-polygon
    [(polygon points)
     (let loop ((rest points)
                (rest^ (cdr points))
                (edges '()))
       (cond
        ((empty? rest^) (reverse (cons
                                  (line-segment (first rest) (first points))
                                  edges)))
        (else
         (loop
          (cdr rest)
          (cdr rest^)
          (cons (line-segment (first rest)
                              (first rest^))
                edges)))))]))

(define (*- a b c d)
  (- (* a b) (* c d)))
(define (-* a b c d)
  (* (- a b) (- c d)))

(define (line-intersection line-1 line-2)
  (match-let*
   ([(line-segment p11 p12) (line->line-segment line-1)]
    [(line-segment p21 p22) (line->line-segment line-2)]
    [(point x1 y1) p11]
    [(point x2 y2) p12]
    [(point x3 y3) p21]
    [(point x4 y4) p22])
   (point (/ (*- (*- x1 y2 y1 x2) (- x3 x4) (- x1 x2) (*- x3 y4 y3 x4))
             (*- (- x1 x2) (- y3 y4) (- y1 y2) (- x3 x4)))
          (/ (*- (*- x1 y2 y1 x2) (- y3 y4) (- y1 y2) (*- x3 y4 y3 x4))
             (*- (- x1 x2) (- y3 y4) (- y1 y2) (- x3 x4))))))

(define (regions-overlap? r11 r12 r21 r22)
  (match-let
   ([(list r11 r12) (sort (list r11 r12) <)]
    [(list r21 r22) (sort (list r21 r22) <)])
   (or (number-in-domain? r11 (domain r21 r22 'inclusive))
       (number-in-domain? r12 (domain r21 r22 'inclusive)))))

(define (domains-overlap? d1 d2)
  (match-let
   ([(domain start1 stop1 _) d1]
    [(domain start2 stop2 _) d2])
   (or (number-in-domain? start1 d2)
       (number-in-domain? stop1  d2)
       (number-in-domain? start2 d1)
       (number-in-domain? stop2  d1))))

(define (line-segment-xs a-line)
  (match-let*
   ([(line-segment p1 p2) a-line]
    [(point x1 _) p1]
    [(point x2 _) p2])
   (list x1 x2)))

(define (line-segment-ys a-line)
  (match-let*
   ([(line-segment p1 p2) a-line]
    [(point _ y1) p1]
    [(point _ y2) p2])
   (list y1 y2)))

(define (line-segment-domains-overlap? l1 l2)
  (match-let
   ([(list x11 x12) (line-segment-xs l1)]
    [(list y11 y12) (line-segment-ys l1)]
    [(list x21 x22) (line-segment-xs l2)]
    [(list y21 y22) (line-segment-ys l2)])
   (and (domains-overlap? (domain x11 x12 'inclusive) (domain x21 x22 'inclusive))
        (domains-overlap? (domain y11 y12 'inclusive) (domain y21 y22 'inclusive)))))

(define (line-segment-intersection ls1 ls2)
  (if (not (line-segment-domains-overlap? ls1 ls2)) #f
      (match-let*
       ([(line-segment p11 p12) ls1]
        [(line-segment p21 p22) ls2]
        [(point x1 y1) p11]
        [(point x2 y2) p12]
        [(point x3 y3) p21]
        [(point x4 y4) p22]
        [(point ix iy) (point (/ (*- (*- x1 y2 y1 x2) (- x3 x4) (- x1 x2) (*- x3 y4 y3 x4))
                                 (*- (- x1 x2) (- y3 y4) (- y1 y2) (- x3 x4)))
                              (/ (*- (*- x1 y2 y1 x2) (- y3 y4) (- y1 y2) (*- x3 y4 y3 x4))
                                 (*- (- x1 x2) (- y3 y4) (- y1 y2) (- x3 x4))))])
       (if
        (and (number-in-domain? ix (domain* x1 x2 'inclusive))
             (number-in-domain? ix (domain* x3 x4 'inclusive))
             (number-in-domain? iy (domain* y1 y2 'inclusive))
             (number-in-domain? iy (domain* y3 y4 'inclusive)))
        (point ix iy)
        #f))))

(define (line-line-segment-intersection li liseg)
  (match (list (or (vertical-line? li) (line? li)) (line-segment? liseg))
    [(list #t #t)
     (mlet* m-maybe
            ((point (line-intersection li (line-segment->line liseg))))
            (if (number-in-domain? (point-x point)
                                   (domain
                                    (point-x (line-segment-p1 liseg))
                                    (point-x (line-segment-p2 liseg))
                                    'inclusive))
                point
                #f))]
    [(list #f #f)
     (line-line-segment-intersection liseg li)]))

(define (line-segment-mid-point a-line)
  (match-let*
   ([(line p1 p2) a-line]
    [(point x1 y1) p1]
    [(point x2 y2) p2])
   (point (/ (+ x1 x2) 2)
          (/ (+ y1 y2) 2))))

(define (point-on-line-segment? p li)
  (match-let*
   ([(point x y) p]
    [(line-segment lp1 lp2) li]
    [(point lx1 ly1) lp1]
    [(point lx2 ly2) lp2])
   (and (number-in-domain? x (domain lx1 lx2 'inclusive))
        (number-in-domain? y (domain ly1 ly2 'inclusive))
        (= y
           (line-at (line-segment->line li) x)))))

(define (point-on-line? p li)
  (= (point-y p)
     (line-at li (point-x p))))

(define (point-in-interior-of-polygon? p poly)
  (let* ((edges (polygon-edges poly))
         (ray (line-segment->line
               (line-segment p (line-segment-mid-point (car edges)))))
         (intersections
          (length
           (filter
            (lambda (edge)
              (line-line-segment-intersection ray edge))
            edges))))
    (if (= 0 (modulo intersections 2)) #f
        #t)))


(define (point-in-polygon? p poly)
  (let ((edges (polygon-edges poly)))
    (or (any-by
         edges (>partial point-on-line-segment? p))
        (point-in-interior-of-polygon? p poly))))

(define (point-in-circle? ci p)
  (match ci
    [(circle center radius)
     (number-in-domain?
      (point-dist center p)
      (domain 0 radius 'inclusive))]))

(define (rectangle-xs r)
  (match r
    [(rectangle p1 p2)
     (list (point-x p1) (point-x p2))]))

(define (rectangle-ys r)
  (match r
    [(rectangle p1 p2)
     (list (point-y p1) (point-y p2))]))

(define (point-in-rectangle? p r)
  (match-let
   ([(list x1 x2) (rectangle-xs r)]
    [(list y1 y2) (rectangle-ys r)])
   (and (number-in-domain?
         (point-x p) (domain* x1 x2 'inclusive))
        (number-in-domain?
         (point-y p) (domain* y1 y2 'inclusive)))))

(define (rectangle->corners r)
  (mlet* m-list
         ((x (rectangle-xs r))
          (y (rectangle-ys r)))
         (m-return (point x y))))

(define (rectangle-top-left r)
  (point (reduce min (rectangle-xs r))
         (reduce min (rectangle-ys r))))

(define (rectangle-bottom-left r)
  (point (reduce min (rectangle-xs r))
         (reduce max (rectangle-ys r))))

(define (rectangle-top-right r)
  (point (reduce max (rectangle-xs r))
         (reduce min (rectangle-ys r))))

(define (rectangle-bottom-right r)
  (point (reduce max (rectangle-xs r))
         (reduce max (rectangle-ys r))))

(define (rectangle->edges r)
  (list (line-segment (rectangle-top-left r) (rectangle-top-right r))
        (line-segment (rectangle-top-right r) (rectangle-bottom-right r))
        (line-segment (rectangle-bottom-right r) (rectangle-bottom-left r))
        (line-segment (rectangle-bottom-left r) (rectangle-top-left r))))

(define (rectangle-edges-cross? r1 r2)
  (let ((r2-edges (rectangle->edges r2)))
    (let loop ((edges (rectangle->edges r1)))
      (cond ((empty? edges) #f)
            ((any-by r2-edges (>partial line-segment-intersection (car edges))) #t)
            (else (loop (cdr edges)))))))

(define (rectangles-overlap? r1 r2)
  (let* ((points-1 (rectangle->corners r1))
         (points-2 (rectangle->corners r2)))
    (or (> (length (filter
                    (partial< point-in-rectangle? r2)
                    points-1)) 0)
        (> (length (filter
                    (partial< point-in-rectangle? r1)
                    points-2)) 0)
        (rectangle-edges-cross? r1 r2))))

(define (point-in-shape? p s)
  (cond
   ((point? s)     (point= s p))
   ((rectangle? s) (point-in-rectangle? p s))
   ((circle? s)    (point-in-circle? p s))
   ((polygon? s)   (point-in-polygon? p s))
   ((or (line? s)
        (vertical-line? s))
    (point-on-line? p s))
   ((line-segment? (point-on-line-segment? p s)))))

(define (feature->string f)
  (match f
    [(point x y) (format "(point ~a ~a)" x y)]
    [(line-segment p1 p2)
     (format "(line-segment ~a ~a)" (feature->string p1) (feature->string p2))]
    [(circle c r)
     (format "(circle ~a ~a)" (feature->string c) r)]
    [(rectangle p1 p2)
     (format "(rectangle ~a ~a)" (feature->string p1) (feature->string p2))]
    [(polygon points)
     (format "(polyon '~a)" (map feature->string points))]
    [(line m x) (format "(line ~a ~a)" m x)]
    [(vertical-line x p) (format "(vertical-line ~a ~a)" x p)]))

(define (radians->point-vector rads)
  (point (cos rads) (sin rads)))

(define (point-vector->radians pt)
  (match (normalize-point pt)
    [(point x y) (atan y x)]))

(define (round-point p)
  (match p
    [(point x y) (point (round x) (round y))]))
  

(provide point point? point-x point-y feature->string radians->point-vector
         rectangle->edges
         point-vector->radians round-point
         line-segment line-segment? line-segment-p1 line-segment-p2
         line line? line-m line-b normalize-point
         vertical-line vertical-line? vertical-line-x vertical-line-positive
         circle circle? circle-r circle-c
         rectangle rectangle? rectangle-p1 rectangle-p2 
         polygon polygon? polygon-points
         domain domain? domain-start domain-stop domain-kind
         domain* number-in-domain? 2pi π 2π point+ point- point.
         point-mag point-dist point-scale point-field-*
         point= line-segment-vertical? line-segment-slope
         line-segment-intercept line-segment->line line-at
         line->pair-of-points line->line-segment line->lambda
         points-xs points-ys make-line-via-regression
         make-lambda-via-regression line-intersection
         regions-overlap? domains-overlap? line-segment-xs
         line-segment-ys line-segment-domains-overlap? line-segment-intersection
         line-line-segment-intersection line-segment-mid-point
         point-on-line-segment? point-on-line? point-in-interior-of-polygon?
         point-in-polygon? point-in-circle? rectangle-xs rectangle-ys
         point-in-rectangle? point-in-shape? rectangle->corners
         rectangles-overlap?
         )



