#lang racket


(require racket/gui
         (prefix-in ss: slideshow/pict)
         (rename-in utilities/proletariat [-> -->])
         (rename-in roguelikes/white-whale-data3 [floor dungeon-floor] [exit dungeon-exit])
         roguelikes/colorations
         roguelikes/grid
         functional/monads
         )

(define square-size 6)
(define circle-size (inexact->exact (* .8 square-size)))

(define (square . args)
  (match args
    [(list r g b)
     (ss:colorize (ss:filled-rectangle square-size square-size)
                  (make-object color% r g b))]
    [(list color)
     (ss:colorize (ss:filled-rectangle square-size square-size)
                  color)]))

(define-multimethod (to-square thing) :: (class-name thing))

(define-method (to-square wall) :: wall
  (square 200 200 200))

(define-method (to-square fluid) :: fluid
  (square (coloration fluid)))

(define (filled-circle r)
  (ss:filled-ellipse r r))

(define-method (to-square dungeon-floor) :: floor
  (let* ((bg (square "black"))
        (entities (at dungeon-floor 'entities))
        (things (at dungeon-floor 'things))
        (fg
         (cond
           ((and (empty? entities) (empty? things))
                 (ss:blank))
           ((empty? entities)
            (ss:colorize (ss:circle square-size)
                         "yellow"))
           (else
            (let ((e (car entities)))
              (if (player? e)
                  (ss:colorize (filled-circle circle-size)
                         "white")
                  (ss:colorize (filled-circle circle-size)
                         "red")))))))
    (ss:cc-superimpose bg fg)))

(define (exactify lst)
  (map inexact->exact lst))

(define-multimethod (render-to-dc dc thing) :: (class-name thing))

(define wall-color (make-object color% 200 200 200))
(define floor-color (make-object color% "black"))
(define object-color (make-object color% "yellow"))
(define player-color (make-object color% "white"))
(define entity-color (make-object color% "red"))

(define-method (render-to-dc dc thing) :: wall
  (send dc set-brush wall-color 'solid)
  (send dc set-pen wall-color 1 'solid)
  (send dc draw-rectangle 0 0 square-size square-size)
  dc)

(define-method (render-to-dc dc thing) :: floor
  (send dc set-brush floor-color 'solid)
  (send dc set-pen wall-color 1 'solid)
  (send dc draw-rectangle 0 0 square-size square-size)
  (let ((entities (at thing 'entities))
        (things (at thing 'things)))
    (match (list entities things)
      [(list (list) (list)) dc]
      [(list (cons e r) _) 
       (render-to-dc dc e)]
      [(list (list) (cons t r))
       (render-to-dc dc t)])))

(define-method (render-to-dc dc thing) :: fluid
  (send dc set-brush (coloration thing) 'solid)
  (send dc set-pen (coloration thing) 1 'solid)
  (send dc draw-rectangle 0 0 square-size square-size)
  (let ((entities (at thing 'entities))
        (things (at thing 'things)))
    (match (list entities things)
      [(list (list) (list)) dc]
      [(list (cons e r) _) 
       (render-to-dc dc e)]
      [(list (list) (cons t r))
       (render-to-dc dc t)])))

(define-method (render-to-dc dc thing) :: object
  (send dc set-brush object-color 'solid)
  (send dc set-pen object-color 1 'solid)
  (send dc draw-ellipse 0 0 square-size square-size))

(define-method (render-to-dc dc thing) :: player
  (send dc set-brush player-color 'solid)
  (send dc set-pen player-color 1 'solid)
  (send dc draw-ellipse 0 0 square-size square-size))

(define-method (render-to-dc dc thing) :: entity 
  (send dc set-brush entity-color 'solid)
  (send dc set-pen entity-color 1 'solid)
  (send dc draw-ellipse 0 0 square-size square-size))
  
(define (minimap-of* grid)
  (match-let* 
      ([(list (list min-x min-y)
              (list max-x max-y))
        (grid-extent grid)]
       [w (- max-x min-x)]
       [h (- max-y min-y)]
       [pxw (* w square-size)]
       [pyh (* h square-size)]
       [bmp (make-object bitmap% 
              (inexact->exact pxw)
              (inexact->exact pyh) #f #t)]
       [dc (new bitmap-dc% [bitmap bmp])])
    (let loop-x 
      [(xi min-x)
       (xc 0)]
      (if (> xi max-x) (ss:bitmap bmp)
          (let loop-y
            [(yi min-y)
             (yc 0)]
            (if (> yi max-y)
                (loop-x (+ xi 1)
                        (+ xc square-size))
                (let ((tile (at grid (exactify (list xi yi)))))
                  ;(display (format "tile ~a, xi ~a, yi ~a, xc ~a, yc ~a~n" tile xi yi xc yc))
                  (if tile 
                      (ss:draw-pict (to-square tile) dc xc yc)
                      #f)
                  (loop-y (+ yi 1)
                          (+ yc square-size)))))))))

(define (minimap-of grid)
  (match-let* 
      ([(list (list min-x min-y)
              (list max-x max-y))
        (grid-extent grid)]
       [w (- max-x min-x)]
       [h (- max-y min-y)]
       [pxw (* w square-size)]
       [pyh (* h square-size)]
       [bmp (make-object bitmap% 
              (inexact->exact pxw)
              (inexact->exact pyh) #f #t)]
       [dc (new bitmap-dc% [bitmap bmp])])
    (send dc set-origin 0 0)
    (let loop-x 
      [(xi min-x)
       (xc 0)]
      (if (> xi max-x) (ss:bitmap bmp)
          (let loop-y
            [(yi min-y)
             (yc 0)]
            (if (> yi max-y)
                (loop-x (+ xi 1)
                        (+ xc square-size))
                (let ((tile (at grid (exactify (list xi yi)))))
                  ;(display (format "tile ~a, xi ~a, yi ~a, xc ~a, yc ~a~n" tile xi yi xc yc))
                  (if tile 
                      (begin
                        (send dc set-origin xc yc)
                        (render-to-dc dc tile))
                      #f)
                  (loop-y (+ yi 1)
                          (+ yc square-size)))))))))
                        
(provide minimap-of minimap-of*)