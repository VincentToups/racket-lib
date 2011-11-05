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

(define (grid->pict grid center img-w-px img-h-px)
  (match-let* 
      ([(list cx cy) center]
       [img-w-tiles (/ img-w-px *tile-size*)]
       [img-h-tiles (/ img-h-px *tile-size*)]
       [i (inexact->exact (floor (- cx (/ img-w-tiles 2))))]
       [i-max (inexact->exact (floor (+ cx (/ img-w-tiles 2))))]
       [j (inexact->exact (floor (- cx (/ img-h-tiles 2))))]
       [j-max (inexact->exact (floor (+ cx (/ img-h-tiles 2))))]
       [bmp (make-object bitmap% img-w-px img-h-px #f #t)]
       [tileize-x (lambda (x) (+ (* x *tile-size*) (/ img-w-px 2)))]
       [tileize-y (lambda (y) (+ (* y *tile-size*) (/ img-h-px 2)))]
       [dc  (new bitmap-dc% [bitmap bmp])])
    (let i-loop ([i i])
      (if (< i i-max)
          (begin 
            (let j-loop ([j j])
              ;(display (format "(~a ~a | ~a ~a)~n" i-max j-max i j))
              (if (< j j-max)
                  (let ([tile (at grid (list i j))])
                    (if tile 
                        (ss:draw-pict (to-pict tile) dc (tileize-x i) (tileize-y j))
                        #f)
                    (j-loop (+ j 1)))
                  #f))
            (i-loop (+ i 1)))
          (ss:bitmap bmp)))))


               

