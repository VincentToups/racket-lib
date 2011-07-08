#lang racket

(require utilities/planar-geometry
         racket/gui
         racket/match)

(define (draw-point dc p)
  (match p
    [(point x y)
     (send dc draw-point x y)]))

(define (draw-line-segment dc liseg)
  (match-let*
   ([(line-segment p1 p2) liseg]
    [(point x1 y1) p1]
    [(point x2 y2) p2])
   (send dc draw-line x1 y1 x2 y2)))

(define (draw-vertical-line-in-rect dc li r)
  (match-let*
   ([(rectangle p1 p2) r]
    [(point x1 y1) p1]
    [(point x2 y2) p2]
    [(vertical-line x p) li])
   (send dc draw-line x y1 x y2)))
    
   
(define (draw-line-in-rect dc li r)
  (cond
   ((line? li)
    (match-let*
     ([(rectangle p1 p2) r]
      [(point x1 y1) p1]
      [(point x2 y2) p2])
     (send dc draw-line (line-at li x1) (line-at li x2))))
   ((vertical-line? li)
    (draw-vertical-line-in-rect dc li r))))

(define (draw-rectangle dc r)
  (match-let*
   ([(rectangle p1 p2) r]
    [(point x1 y1) p1]
    [(point x2 y2) p2]
    [(list x1 x2) (sort (list x1 x2) <)]
    [(list y1 y2) (sort (list y1 y2) <)]
    [w (- x2 x1)]
    [h (- y2 y1)])
   (send dc draw-rectangle x1 y1 w h)))

(define (draw-circle dc c)
  (match-let*
   ([(circle center radius) c]
    [(point x y) center])
   (send dc draw-arc (- x  (/ radius 2))
         (- y (/ radius 2)) 
         radius radius 0 (* 2 pi))))

(define (draw-polygon dc p)
  (match-let* 
   ([(polygon points) p])
   (send dc draw-lines
         (map (lambda (p)
                (match p
                  [(point x y) (list x y)]))
              points))))

(define (draw-shape dc s . args)
  (cond
   ((point? s) (draw-point dc s))
   ((line-segment? s) (draw-line-segment dc s))
   ((or (line? s)
        (vertical-line? s))
    (apply draw-line-in-rect dc s args))
   ((circle? s) (draw-circle dc s))
   ((rectangle? s) (draw-rectangle dc s))
   ((polygon? s) (draw-polygon dc s))
   (else
    (error (format "Can't draw ~a" s)))))
                
(provide (all-defined-out))

