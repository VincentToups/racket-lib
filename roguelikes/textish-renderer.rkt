#lang racket
(require racket/gui
         (prefix-in ss: slideshow/pict)
         (rename-in utilities/proletariat [-> -->])
         (rename-in roguelikes/white-whale-data3 [floor dungeon-floor] [exit dungeon-exit])
         roguelikes/colorations
         roguelikes/grid
         functional/monads
         roguelikes/character-representations)

(define (pict-width p)
  (match p
   [(struct* ss:pict [(width w)])
     w]))

(define (pict-height p)
  (match p
    [(struct* ss:pict [(height h)])
     h]))

(define (char c bgc fgc sz)
  (let* ((fg (ss:colorize (ss:text c (make-object font% 11 'swiss)) fgc))
         (bg (ss:colorize (ss:filled-rectangle sz sz) bgc)))
    (ss:cc-superimpose bg fg)))

(define-multimethod (to-pict object) :: (class-name object))

(define *tile-size* 16)

(define-method (to-pict object) :: object
  (char (character object) (background-coloration object) (coloration object) 16))

(define (tiles-per-pixel npix)
  (inexact->exact (ceiling (/ npix *tile-size*))))

(define (tile-range-in-pixel-range n-pix center-tile)
  (let ((n-tiles (tiles-per-pixel n-pix)))
    (list
     (inexact->exact (- center-tile (ceiling (/ n-tiles 2))))
     (inexact->exact (+ center-tile (ceiling (/ n-tiles 2)))))))

(define (grid->pict grid center-tile pxw pxh)
  (match-let* 
      ([(list tilecx tilecy) (map inexact->exact center-tile)]
       [(list min-x max-x) (tile-range-in-pixel-range pxw tilecx)]
       [(list min-y max-y) (tile-range-in-pixel-range pxh tilecy)]
       [bmp (make-object bitmap% (inexact->exact pxw) (inexact->exact pxh) #f #t)]
       [dc (new bitmap-dc% [bitmap bmp])])
    (let loop-x ((tile-x min-x)
                 (coord-x 0))
      (if (> tile-x max-x)
          (ss:bitmap bmp)
          (let loop-y ((tile-y min-y)
                       (coord-y 0))
            (if (> tile-y max-y) (loop-x (+ tile-x 1) (+ coord-x *tile-size*))
                (let ([tile (at grid (list tile-x tile-y))])
                  (if tile
                      (ss:draw-pict (to-pict tile) dc coord-x coord-y)
                      #f)
                  (loop-y (+ tile-y 1) (+ coord-y *tile-size*)))))))))

(define (mean . args)
  (match (foldl (match-lambda* 
           [(list (? number? n)
                  (list count sum))
            (list (+ count 1) 
                  (+ sum n))])
                (list 0 0)
                args)
    [(list count sum)
     (/ sum count)]))

(define-method (to-pict grid) :: grid
  (if (empty-grid? grid) (ss:rectangle *tile-size* *tile-size*)
  (match grid
    [(obj 
      ('min-x min-x)
      ('max-x max-x)
      ('min-y min-y)
      ('max-y max-y))
     (let* ((cx (floor (mean min-x max-x)))
            (cy (floor (mean min-y max-y)))
            (w (* *tile-size* (- max-x min-x)))
            (h (* *tile-size* (- max-y min-y))))
       
       (grid->pict grid (list cx cy) w h))])))
            

(provide to-pict grid->pict)