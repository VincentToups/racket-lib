#lang racket

(require functional/point-free)
(require utilities/lists)

(define (!= a b) (not (= a b)))

(define (local-any . args) 
  (any args))


(define (member-if pred lst)
  #|
    proc member-if pred
    returns the sublist whose car is the first element of lst
    for which (pred el) == #t
    |#
  (let loop ((rst lst))
    (cond
      ((null? rst) #f)
      ((pred (car rst)) rst)
      (#t (loop (cdr rst))))))


(define (elt lst ix)
  (let loop ((i 0)
             (lst lst))
    (cond 
      ((empty? lst) (error "Index out of range."))
      ((= i ix) (car lst))
      (else
       (loop (+ i 1)
             (cdr lst))))))

(define-syntax (named-fn stx)
  (syntax-case stx ()
    [(named-fn name args body ...)
     (syntax
      (let ((name (lambda args body ...)))
        name))]))

(define-syntax (fn stx)
  (syntax-case stx ()
    [(fn args body ...)
     (syntax (lambda args body ...))]))

(define-syntax (sum-of stx)
  (syntax-case stx (<-)
    [(sum-of expr (var <- list))
     (syntax (reduce (lambda (ac var)
                       (+ ac expr)) list))]))


(define 2pi (* 2 pi))
(define ^ expt)

(define make-point list)
(define pt! make-point)
(define (point? o)
  (and
   (list? o)
   (all (map number? o))))
(define pt? point?)
(define (norm-pt pt)
  (map (partial< / (pt<> pt)) pt))
(define (pt+ . args)
  (apply map + args))
(define (pt- . args)
  (apply map - args))
(define (pt. p1 p2)
  (apply + (map * p1 p2)))
(define (pt<> pt)
  (sqrt (apply + (map (partial< ^ 2) pt))))
(define (pt|| pt1 pt2)
  (pt<> (pt- pt1 pt2)))

(define (pt* a b)
  (cond ((and (number? a) (point? b))
         (map (partial< * a) b))
        ((and (point? a) (number? b))
         (map (partial< * b) a))
        (#t
         (error "Can't pt* these quantities"))))

(define (make-line x1 y1 . args)
  (cond ((= (length args) 0)
         (list x1 y1));; In this case x1 and y1 are points
        ((= (length args) 2)
         (list (make-point x1 y1)
               (list->point args)))));; Here we are the simple constructor;
(define line! make-line)
(define ln! make-line)
(define (line x1 y1 x2 y2)
  (display "line is deprecated for making lines, use line! instead\n")
  (line! x1 y1 x2 y2))

(define (line-direction line)
  (norm-pt (pt- (line-pt-2 line)
                (line-pt-1 line))))

(define (xy-at-d-along-line-from line distance from)
  (let ((vec (norm-pt
              (pt- (line-pt-2 line)
                   (line-pt-1 line)))))
    (pt+ from (pt* distance vec))))

(define (make-circle center radius)
  (list 'circle center radius))
(define circle! make-circle)
(define circ! make-circle)
(define (circle? obj)
  (eq? (car obj) 'circle))
(define circ? circle?)
(define (circle-center circ)
  (cadr circ))
(define (circle-radius circ)
  (caddr circ))

(define (circle-circumference circ)
  (* 2pi (circle-radius circ)))

(define (point-angle pt)
  (atan (pt-y pt) (pt-x pt)))

(define (xy-at-d-along-circle-from circle distance from)
  (let* ((ang-from (angle-on-circle circle from))
         (ang-sweep (* 2pi (/ distance (circle-circumference circle))))
         (ang-to (+ ang-from ang-sweep)))
    (point-on-circle-at-angle circle ang-to)))

(define (circle-bounding-box circ)
  (let* ((rad (circle-radius circ))
         (cen (circle-center circ))
         (bottom-left
          (pt- cen (pt* (pt! 1. 1.) rad)))
         (top-right
          (pt+ bottom-left (pt* (* 2 rad) (pt! 1. 1.)))))
    (list bottom-left top-right)))

(define (angle-on-circle circle pt)
  (let ((pt (pt- pt (circle-center circle))))
    (point-angle pt)))

(define (point-on-circle-at-angle circle ang)
  (pt+ (circle-center circle) (pt* (circle-radius circle)
                                   (make-point (cos ang) (sin ang)))))

(define (3-points->circle-find-center p1 p2 p3)
  (let* ((a (make-line p1 p2))
         (b (make-line p2 p3))
         (ma (line-slope a))
         (mb (line-slope b))
         (xc (/
              (+ (* ma mb (- (pt-y p1) (pt-y p3)))
                 (* mb (+ (pt-x p1) (pt-x p2)))
                 (- (* ma (+ (pt-x p2) (pt-x p3)))))
              (* 2 (- mb ma))))
         (yc (- (/ (+ (pt-y p1) (pt-y p2)) 2)
                (* (/ 1 ma) (- xc (/ (+ (pt-x p1) (pt-x p2)) 2))))))
    (pt! xc yc)))

(define (circle->lambda circle)
  (named-fn circle-fn (theta)
            (point-on-circle-at-angle circle theta)))           

(define (3-points->circle p1 p2 p3)
  (let* ((center (3-points->circle-find-center p1 p2 p3)))
    (make-circle center (point-distance center p1))))

(define (point-distance p1 p2)
  (sqrt (apply + (map (partial< ^ 2) (map - p1 p2)))))

(define (make-piecewise-line . points)
  (let ((corrected-pnts
         (cons (car points)
               (reverse
                (cdr
                 (reverse
                  (rep-list
                   2
                   (cdr points))))))))
    (bunch-by (map
               (lambda (pnts) (apply make-point pnts))
               corrected-pnts) 2)))

(define (pwl-get-piece pwl i)
  (elt pwl i))

(define (piecewise-line-nth-pt pw-line n)
  (let ((pts (append (car pw-line)
                     (map line-pt-2 (cdr pw-line)))))
    (elt pts n)))

(define (line-intersects-pw-line line pw-line)
  (map (lambda (piece)
         (lines-intersection piece line))
       pw-line))

(define (inexactify-line ln)
  (map (lambda (pt) (map exact->inexact pt)) ln))

(define (list->point li)
  li)

(define (line-pt-1 li)
  (car li))
(define (line-pt-2 li)
  (cadr li))

(define (line? o)
  (and (list? o)
       (all (map point? o))))

(define ln? line?)

(define (line-horizontal? line)
  (= (cadr (car line))
     (cadr (cadr line))))
(define (line-vertical? line)
  (= (car (car line))
     (car (cadr line))))

(define (line-slope li)
  (if (line-vertical? li) 'inf
      (let ((p1 (line-pt-1 li))
            (p2 (line-pt-2 li)))
        (/ (- (point-y p1) (point-y p2))
           (- (point-x p1) (point-x p2))))))

(define (line-intercept li)
  (if (line-vertical? li) 'NaN
      (let* ((p (line-pt-1 li))
             (x (point-x p))
             (y (point-y p)))
        (- y (* (line-slope li) x)))))

(define (line->lambda li)
  (if (line-vertical? li)
      (lambda (x) (point-x (line-pt-1 li)))
      (let ((m (line-slope li))
            (b (line-intercept li)))
        (lambda (x) (+ (* m x) b)))))

(define (make-points xs ys)
  (map make-point xs ys))
(define pts! make-points)

(define (point-x point)
  (car point))
(define (point-y point)     
  (cadr point))

(define pt-x point-x)
(define pt-y point-y)

(define (make-line-via-regression . points)
  (let ((points (if (= 1 (length points)) (car points) points)))
    (let* ((points (sort points (fn (p1 p2) (< (point-x p1) (point-y p2)))))
           (m (length points))
           (xs (map point-x points))
           (ys (map point-y points))
           (sx (sum-of x (x <- xs)))
           (sy (sum-of y (y <- ys)))
           (sx2 (sum-of (* x x) (x <- xs)))
           (sxy (sum-of xy (xy <- (map * xs ys))))
           (d (- (* m sx2) (* sx sx)))
           (intercept
            (/ (- (* sx2 sy) (* sx sxy)) d))
           (slope
            (/ (- (* m sxy) (* sx sy)) d))
           (x-start (apply min xs))
           (x-end (apply max xs)))
      (make-line (make-point
                  x-start
                  (+ intercept (* slope x-start)))
                 (make-point
                  x-end
                  (+ intercept (* slope x-end)))))))

(define (make-fn-via-regression . points)
  (let ((points (if (= 1 (length points)) (car points) points)))
    (line->lambda (apply make-line-via-regression points))))

(define (get-x-interval x xs ys)
  (let loop ((rst-x xs)
             (rst-y ys))
    (cond ((null? rst-x) #f)
          ((and (> x (car rst-x)) (<= x (cadr rst-x)))
           (values (make-point (car rst-x)
                               (car rst-y))
                   (make-point (cadr rst-x)
                               (cadr rst-y))))
          (else
           (loop (cdr rst-x)
                 (cdr rst-y))))))

(define (points->interp-fn . points)
  (let* ((points
          (sort (if (= (len points) 1) (car points) points)
                (lambda (e1 e2) (< (pt-x e1) (pt-x e2)))))
         (lines
          (reverse (foldl
                    (fn (current-point lines)
                        (cons (line! (line-pt-2 (car lines))
                                     current-point) lines))
                    (list (line! (car points) (cadr points)))
                    (cddr points))))
         (last-point (car (reverse points)))
         (last-line (car (reverse lines))))
    (named-fn interp-fn (x)
              (cond ((< x (pt-x (car points)))
                     ;(disp "Before")
                     (let ((m (line-slope (car lines)))
                           (b (line-intercept (car lines))))
                       (+ b (* m x))))
                    ((> x (pt-x last-point))
                     ;(dispf "After ~a" (pt-x last-point))
                     (let ((m (line-slope last-line))
                           (b (line-intercept last-line)))
                       (+ b (* m x))))
                    (else
                     ;(disp "Within")
                     (let* ((line (car (member-if (>partial within-line-domain? x) lines)))
                            (m (line-slope line))
                            (b (line-intercept line)))
                       (+ b (* m x))))))))

(define (point-in-box? box point)
  (let* ((x (car point))
         (y (cadr point))
         (boxx (map point-x box))
         (boxy (map point-y box))
         (xmin (apply min boxx))
         (xmax (apply max boxx))
         (ymin (apply min boxy))
         (ymax (apply max boxy)))
    (and (>= x xmin)
         (<= x xmax)
         (>= y ymin)
         (<= y ymax))))

(define (sign x)
  (inexact->exact (/ x (abs x))))

(define (box-0 f xl xr tol)
  (display "box-0 is deprecated.  Don't use it!")
  (newline)
  (find-zeros-bracket f xl xr tol))

(define (find-zeros-bracket  f xl xr tol)
  (let* ((fl (f xl))
         (fr (f xr))
         (xm (/ (+ xr xl) 2.0))
         (fm (f xm)))
    (cond ((= (sign fl)
              (sign fr))
           (error "find-zeros-bracket You failed to bracket the root"))
          ((= 0.0 fl) xl)
          ((= 0.0 fr) xr)
          ((< (abs fm) tol) xm)
          (else
           ;;choose new bracket
           (cond
             ((!= (sign fl) (sign fm))
              (find-zeros-bracket f xl xm tol))
             ((!= (sign fr) (sign fm))
              (find-zeros-bracket f xm xr tol)))))))

(define (box-intersect? box-a box-b)
  (apply 
   local-any
   (append 
    (map (lambda (point) (point-in-box? box-a point))
         box-b)
    (map (lambda (point) (point-in-box? box-b point))
         box-a))))

(define (within-line-domain? x line)
  (let ((xs (map point-x line)))
    (and (>= x (apply min xs))
         (<= x (apply max xs)))))

(define (within-line-range? y line)
  (let ((ys (map point-y line)))
    (and (>= y (apply min ys))
         (<= y (apply max ys)))))

(define (lines-intersect? line-a line-b)
  (let* ((x1 (car (car line-a)))
         (x2 (car (cadr line-a)))
         (x3 (car (car line-b)))
         (x4 (car (cadr line-b)))
         (y1 (cadr (car line-a)))
         (y2 (cadr (cadr line-a)))
         (y3 (cadr (car line-b)))
         (y4 (cadr (cadr line-b)))
         (denom (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))
         (ua (/ (- (* (- x4 x3) (- y1 y3))
                   (* (- y4 y3) (- x1 x3))) denom))
         (xint (+ x1 (* ua (- x2 x1))))
         (yint (+ y1 (* ua (- y2 y1)))))
    (and (within-line-domain? xint line-a)
         (within-line-domain? xint line-b)
         (within-line-range? yint line-a)
         (within-line-range? yint line-b))))

(define (lines-intersection line-a line-b)
  (let* ((x1 (car (car line-a)))
         (x2 (car (cadr line-a)))
         (x3 (car (car line-b)))
         (x4 (car (cadr line-b)))
         (y1 (cadr (car line-a)))
         (y2 (cadr (cadr line-a)))
         (y3 (cadr (car line-b)))
         (y4 (cadr (cadr line-b)))
         (denom (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))
         (ua (/ (- (* (- x4 x3) (- y1 y3))
                   (* (- y4 y3) (- x1 x3))) denom))
         (xint (+ x1 (* ua (- x2 x1))))
         (yint (+ y1 (* ua (- y2 y1)))))
    (if (and (or (line-vertical? line-a) (within-line-domain? xint line-a))
             (or (line-vertical? line-b) (within-line-domain? xint line-b))
             (or (line-horizontal? line-a) (within-line-range? yint line-a))
             (or (line-horizontal? line-b) (within-line-range? yint line-b)))
        (list xint yint)
        #f)))

(define point! pt!)

(provide make-point
        pt!
        point!
        point?
        pt?
        norm-pt
        pt+
        pt-
        pt.
        pt<>
        pt*
        pt||
        make-line
        line!
        ln!
        line-direction
        xy-at-d-along-line-from
        make-circle
        circle!
        circ!
        circle?
        circ?
        circle-center
        circle-radius
        circle-circumference
        point-angle
        xy-at-d-along-circle-from
        circle-bounding-box
        angle-on-circle
        point-on-circle-at-angle
        3-points->circle-find-center
        circle->lambda
        3-points->circle
        point-distance
        make-piecewise-line
        pwl-get-piece
        piecewise-line-nth-pt
        line-intersects-pw-line
        inexactify-line
        list->point
        line-pt-1
        line-pt-2
        line?
        line-horizontal?
        line-vertical?
        line-slope
        line-intercept
        line->lambda
        make-points
        pts!
        point-x
        point-y
        pt-x
        pt-y
        make-line-via-regression
        make-fn-via-regression
        get-x-interval
        points->interp-fn
        point-in-box?
        sign
        box-0
        find-zeros-bracket
        box-intersect?
        within-line-domain?
        within-line-range?
        lines-intersect?
        lines-intersection)