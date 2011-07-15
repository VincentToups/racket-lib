#lang racket
(require racket/match
         functional/point-free
         functional/monads
         utilities/lists
         )


;;; Data Types: Each call to struct (defined in racket/base) creates a
;;; constructor, predicate, and functions to fetch and modify the
;;; fields.  This library never modifies structs, however.  Functions
;;; return new structs with the appropriate changes.  Each struct is
;;; provided with a custom printer which makes the structs
;;; homo-iconic.  This makes interactive work easier.

(struct point (x y)
        ;;; Represents two dimensional points and vectors.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display
           (format "(point ~s ~s)"
                   (point-x s)
                   (point-y s))
           port)))

(struct line-segment (p1 p2)
        ;;; Represents a line segment, constructed from two points, p1
        ;;; and p2.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(line-segment " port)
          (display (line-segment-p1 s) port)
          (display " " port)
          (display (line-segment-p2 s) port)
          (display ")" port)))

(struct line (m b)
        ;;; Represents all infinite lines that are not vertical, with
        ;;; slope m and intercept b.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(line ~s ~s)" (line-m s) (line-b s))))

(struct vertical-line (x positive)
        ;;; Represents a vertical line.
        ;;; In another language, line and vertical line might be a single type with two constructors.
        ;;; They are used in a similar way in this library.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display (format "(vertical-line ~s ~s)" (vertical-line s) (vertical-line-positive s)) port)))

(struct circle (c r)
        ;;; Represents a circle.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display (format "(circle ") port)
          (display (circle-c s) port)
          (display (circle-r s) port)
          (display ")" port)))

(struct rectangle (p1 p2)
        ;;; Represents an axis-oriented rectangle with opposing corners and points P1 and P2.
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(rectangle " port)
          (display (rectangle-p1 s) port)
          (display " " port)
          (display (rectangle-p2 s) port)
          (display ")" port)))

(struct polygon (points)
        ;;; Polygon - a shape defined by an arbitrary list of points.
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
        ;;; Represents a one dimensional domain or set of points
        ;;; between start and stop.  Kind is a symbol specifying
        ;;; whether the left and right edges of the set are open or
        ;;; closed.
        ;;; Must be one of:
        ;;;        exclusive
        ;;;        left-inclusive
        ;;;        right-inclusive
        ;;;        inclusive
        ;;;        left-exclusive
        ;;;        right-exclusive
        ;;;
        ;;; For instance, `left-inclusive` implies that the left
        ;;; boundary of the set is closed, but the right is open.
        #:property prop:custom-write
        (lambda (s port mode)
          (display (format "(domain ~s ~s '~s)"
                           (domain-start s)
                           (domain-stop s)
                           (domain-kind s))
                   port)))


(define (domain* a b kind)
  ;;; Domain constructor which enforces that start < stop.
  (match (sort (list a b) <)
    [(list a b) (domain a b kind)]))

(define (number-in-domain? n a-domain)
  ;;; Returns #t when n is in the domain a-domain.
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


;;; aka tau
(define 2pi (* 2 pi))

;;; Cute:
(define π pi)
(define 2π (* 2 pi))

(define (point+ p1 p2)
  ;;; Add the x and y parts of points p1 and p2, return a new point.
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (point (+ x1 x2)
                    (+ y1 y2))))

(define (point- p1 p2)
  ;;; Subtract the x and y parts of points p1 and p2, return a new point.
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (point (- x1 x2)
                    (- y1 y2))))

(define (point. p1 p2)
  ;;; Take the sum of the pair-wise product of the components of p1
  ;;; and p2, aka the dot product.
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
             (+ (* x1 x2) (* y1 y2))))

(define point-mag
  ;;; Scalar distance of a point from (point 0 0).
  (match-lambda [(point x y)
                 (sqrt (+ (* x x) (* y y)))]))

(define (point-dist p1 p2)
  ;;; Scalar distance between two points p1 and p2.
  (point-mag (point- p1 p2)))

(define (point-scale p s)
  ;;; Adjust the length of the point p (as a vector) by a factor s.
  (match p
    [(point x y) (point (* x s) (* y s))]))

(define (normalize-point p)
  ;;; Scale the length of point p (as a vector) so that it is 1.
  (let ((m (point-mag p)))
    (point-scale p (/ 1 m))))

(define (point-field-* p1 p2)
  ;;; Take the field-product of p1 and p2, return a new point.  This
  ;;; operation is the same as complex multiplication when the y
  ;;; components of p1 and p2 are treated as the imaginary components.
  (match-let
   ([(point x1 y1) p1]
    [(point x2 y2) p2])
   (point (- (* x1 x2)
             (* y1 y2))
          (+ (* x1 y2)
             (* y1 x2)))))

(define (point= p1 p2)
  ;;; Test for equality between p1 and p2.
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(define (line-segment-vertical? a-line-seg)
  ;;; Test whether a line *segment* is vertical.
  (match a-line-seg
    [(line-segment p1 p2)
     (= (point-x p1) (point-y p2))]))

(define (line-segment-slope a-line-seg)
  ;;; Calculate the slop of a line segment.  For non-vertical lines,
  ;;; this is simply m, but for vertical lines an attempt is made to
  ;;; assign a meaningful value to the slop depending on the order p1
  ;;; and p2 in the line segment.
  ;;; If the lower point is first, the slope is +inf.f
  ;;; If the higher point is first, the slop is -inf.f
  ;;; If the points overlap the slope is +nan.f
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
  ;;; Calculate the line segment's y intercept, if it were a line and if one exists.
  ;;; If the line is vertical, then:
  ;;; The lower point is first in the segment, the y intercept is +inf.f
  ;;; if the higher point is first, the y intercept is -inf.f
  ;;; otherwise, the intercept is +nan.f
  (match-let* ([(line-segment p1 p2) a-line-seg]
               [(point x1 y1) p1]
               [(point x2 y2) p2])
              (if (line-segment-vertical? a-line-seg)
                  (cond ((= y1 y2) +nan.f)
                        ((< y1 y2) +inf.f)
                        ((> y1 y2) -inf.f))
                  (- y1 (* (line-segment-slope a-line-seg) x1)))))

(define (line-segment->line line-seg)
  ;;; Convert a line segment to an infinite line.
  (if (line-segment-vertical? line-seg)
      (vertical-line (point-x (line-segment-p1 line-seg))
                     (positive? (line-segment-slope line-seg)))
      (line (line-segment-slope line-seg)
            (line-segment-intercept line-seg))))

(define (line-at a-line x)
  ;;; Return the value y value associated with x by the line a-line.
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
  ;;; Return an arbitrary pair of points associated with a-line.
  (match a-line
    [(line m x) (list (point 0 (line-at a-line 0)) (point 1 (line-at a-line 1)))]
    [(vertical-line lx p)
     (match p
       [#t (list (point lx 0) (point lx 1))]
       [#f (list (point lx 0) (point lx -1))])]))

(define (line->line-segment a-line)
  ;;; Return an arbitrary line segment associated with a-line.
  (match a-line
    [(line m x) (line-segment (point 0 (line-at a-line 0)) (point 1 (line-at a-line 1)))]
    [(vertical-line lx p)
     (match p
       [#t (line-segment (point lx 0) (point lx 1))]
       [#f (line-segment (point lx 0) (point lx -1))])]))

(define (line->lambda line)
  ;;; Return a lambda representing the line.  This is a function which
  ;;; takes x and returns y.
  (>partial line-at line))

(define points-xs
  ;;; Return the x values of a list of points.
  (f-map point-x))

(define points-ys
  ;;; Return the y values of a list of points.
  (f-map point-y))

(define (sort-points-on-xs points)
  ;;; Sort a list of points on their x values.
  (sort points (dec-all < point-x)))

(define (sort-points-on-ys points)
  ;;; Sort a list of points on their y values.
  (sort points (dec-all < point-y)))


(define (square x)
  ;;; Square a number.  Local utility function.
  (* x x))

(define (make-line-via-regression points)
  ;;; Given a list of points, do a linear least squares regression to
  ;;; find the line which minimizes the sum of squared error between
  ;;; predicted and specified y values, given the xs.
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
  ;;; Returns the lambda-version of the linear regression of points.
  (line->lambda (make-line-via-regression points)))

(define (polygon-edges a-polygon)
  ;;; Get the edges of a polygon as a list of line-segments.
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
  ;;; Take a b c and d and return
  ;;; a*b-c*d.
  (- (* a b) (* c d)))
(define (-* a b c d)
  ;;; Take a b c and d and return
  ;;; (a-b)*(c-d)
  (* (- a b) (- c d)))

(define (lines-parallel? line-1 line-2)
  (match line-1
    [(line m1 _)
     (match line-2
       [(line m2 _)
        (= m1 m2)]
       [(vertical-line _ _)
        #f])]
    [(vertical-line _ _)
     (match line-2
       [(line _ _)
        #f]
       [(vertical-line _ _)
        #t])]))

(define (line-intersection line-1 line-2)
  ;;; Find the intersection between the two lines line1 and line2
  ;;; If the lines are parallel, return false.
  (if (lines-parallel? line-1 line-2)
      #f
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
                 (*- (- x1 x2) (- y3 y4) (- y1 y2) (- x3 x4)))))))

(define (regions-overlap? r11 r12 r21 r22)
  ;;; Return #t if regions overlap, #f otherwise.  Regions are assumed
  ;;; to be inclusive/closed sets.
  (match-let
   ([(list r11 r12) (sort (list r11 r12) <)]
    [(list r21 r22) (sort (list r21 r22) <)])
   (or (number-in-domain? r11 (domain r21 r22 'inclusive))
       (number-in-domain? r12 (domain r21 r22 'inclusive)))))

(define (domains-overlap? d1 d2)
  ;;; Returns true if domains d1 and d2 overlap.
  (match-let
   ([(domain start1 stop1 _) d1]
    [(domain start2 stop2 _) d2])
   (or (number-in-domain? start1 d2)
       (number-in-domain? stop1  d2)
       (number-in-domain? start2 d1)
       (number-in-domain? stop2  d1))))

(define (line-segment-xs a-line)
  ;;; Get the x values associated with a line segment.
  (match-let*
   ([(line-segment p1 p2) a-line]
    [(point x1 _) p1]
    [(point x2 _) p2])
   (list x1 x2)))

(define (line-segment-ys a-line)
  ;;; Get the y values associated with a line segment.
  (match-let*
   ([(line-segment p1 p2) a-line]
    [(point _ y1) p1]
    [(point _ y2) p2])
   (list y1 y2)))

(define (line-segment-domains-overlap? l1 l2)
  ;;; Returns true if both the x and y domains of line segments l1 and l2 overlap.
  ;;; Domains are assumed to be inclusive/closed sets.
  (match-let
   ([(list x11 x12) (line-segment-xs l1)]
    [(list y11 y12) (line-segment-ys l1)]
    [(list x21 x22) (line-segment-xs l2)]
    [(list y21 y22) (line-segment-ys l2)])
   (and (domains-overlap? (domain x11 x12 'inclusive) (domain x21 x22 'inclusive))
        (domains-overlap? (domain y11 y12 'inclusive) (domain y21 y22 'inclusive)))))

(define (line-segment-intersection ls1 ls2)
  ;;; Return the intersection of line-segments ls1 and ls2, #f is they do not.
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
  ;;; Returns the intersection between a line li and a line-segment
  ;;; liseg, if such an intersection exists.
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
  ;;; Returns the mid-point of a line-segment.
  (match-let*
   ([(line p1 p2) a-line]
    [(point x1 y1) p1]
    [(point x2 y2) p2])
   (point (/ (+ x1 x2) 2)
          (/ (+ y1 y2) 2))))

(define (point-on-line-segment? p li)
  ;;; Returns true when the point p is on the line-segment li.
  ;;; #f otherwise.
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
  ;;; Returns true when p's y value is the same as the value of li at
  ;;; p's x value.  That is, when p is on li.
  (= (point-y p)
     (line-at li (point-x p))))

(define (point-in-interior-of-polygon? p poly)
  ;;; Returns true when p is in the interior of the polygon poly.
  ;;; This function casts rays out from p in the direction of the mid
  ;;; point of each line segment in poly and counts the intersections.
  ;;; Points interior will intersect with an odd number of edges,
  ;;; points exterior an even.  A point must be strictly in the
  ;;; interior for this method to be true, not merely on a line
  ;;; segment of the polygon.
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
  ;;; Like point-in-interior-of-polygon? except also returns true if p
  ;;; is on the edge of a polygon.
  (let ((edges (polygon-edges poly)))
    (or (any-by
         edges (>partial point-on-line-segment? p))
        (point-in-interior-of-polygon? p poly))))

(define (point-in-circle? p ci)
  ;;; Returns true when the point p lies within the circle ci or is on
  ;;; that circles boundary.
  (match ci
    [(circle center radius)
     (number-in-domain?
      (point-dist center p)
      (domain 0 radius 'inclusive))]))

(define (rectangle-xs r)
  ;;; Return the x-values of the points at the corners of rectangle r.
  (match r
    [(rectangle p1 p2)
     (list (point-x p1) (point-x p2))]))

(define (rectangle-ys r)
  ;;; Return the y-values of the points at the corners of rectangle r.
  (match r
    [(rectangle p1 p2)
     (list (point-y p1) (point-y p2))]))

(define (point-in-rectangle? p r)
  ;;; Return's true when p is inside the rectangle r or on its border.
  (match-let
   ([(list x1 x2) (rectangle-xs r)]
    [(list y1 y2) (rectangle-ys r)])
   (and (number-in-domain?
         (point-x p) (domain* x1 x2 'inclusive))
        (number-in-domain?
         (point-y p) (domain* y1 y2 'inclusive)))))

(define (rectangle->corners r)
  ;;; Get the four corners of the rectangle r.
  (mlet* m-list ;;; monadic-let, see functional/monads
         ((x (rectangle-xs r))
          (y (rectangle-ys r)))
         (m-return (point x y))))

(define (rectangle-top-left r)
  ;;; Return the top-left corner of r as a point.
  (point (reduce min (rectangle-xs r))
         (reduce min (rectangle-ys r))))

(define (rectangle-bottom-left r)
  ;;; Return the bottom-left corner of r as a point.
  (point (reduce min (rectangle-xs r))
         (reduce max (rectangle-ys r))))

(define (rectangle-top-right r)
  ;;; Return the top-right corner of r as a point.
  (point (reduce max (rectangle-xs r))
         (reduce min (rectangle-ys r))))

(define (rectangle-bottom-right r)
  ;;; Return the bottom-right corner of r as a point.
  (point (reduce max (rectangle-xs r))
         (reduce max (rectangle-ys r))))

(define (rectangle->edges r)
  ;;; Return the edges of the rectangle r as a list of line-segments.
  (list (line-segment (rectangle-top-left r) (rectangle-top-right r))
        (line-segment (rectangle-top-right r) (rectangle-bottom-right r))
        (line-segment (rectangle-bottom-right r) (rectangle-bottom-left r))
        (line-segment (rectangle-bottom-left r) (rectangle-top-left r))))

(define (rectangle-edges-cross? r1 r2)
  ;;; Returns #t when the edges of r1 and r2 cross.
  (let ((r2-edges (rectangle->edges r2)))
    (let loop ((edges (rectangle->edges r1)))
      (cond ((empty? edges) #f)
            ((any-by r2-edges (>partial line-segment-intersection (car edges))) #t)
            (else (loop (cdr edges)))))))

(define (rectangles-overlap? r1 r2)
  ;;; Returns true if r1 and r2 overlap in any way.
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
  ;;; Returns true if point p is in shape s, where shape is one of
  ;;; point, rectangle, circle, polygon, line, vertical-line or
  ;;; line-segment.
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
  ;;; Converts a planar geometry element to a string.  Superceded by
  ;;; custom-write properties of the structures.
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
  ;;; Create a vector (as a point) with unit length pointing in the
  ;;; direction specified in radians by rads.
  (point (cos rads) (sin rads)))

(define (point-vector->radians pt)
  ;;; Return a value in radians indicating the direction that a vector
  ;;; (represented as a point) is pointing in.
  (match (normalize-point pt)
    [(point x y) (atan y x)]))

(define (round-point p)
  ;;; Round the x and y coordinates of the point p.
  (match p
    [(point x y) (point (round x) (round y))]))

(define (shape-type s)
  ;;; Return the shape type, as a symbol, of s.
  (match s
    [(? point?) 'point]
    [(? line-segment?) 'line-segment]
    [(? line?) 'line]
    [(? vertical-line?) 'vertical-line]
    [(? polygon?) 'polygon]
    [(? circle?) 'circle]
    [(? rectangle?) 'rectangle]))


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
         shape-type
         rectangles-overlap?
         sort-points-on-xs
         sort-points-on-ys
         lines-parallel?
         points-xs
         points-ys
         )



