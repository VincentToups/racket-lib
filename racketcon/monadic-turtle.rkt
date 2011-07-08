#lang racket

(require 
 racket/gui
 racket/match
 racket/dict
 (rename-in (only-in slideshow/pict bitmap) [bitmap bitmap->pict])
 utilities/planar-geometry
 utilities/fancy-destructuring
 utilities/draw-planar-geometry)

(struct doublet (a b)
  #:property 
  prop:custom-write 
  (lambda (s port mode)
    (display "(doublet " port)
    (display (doublet-a s) port)
    (display " " port)
    (display (doublet-b s) port)
    (display ")" port)))
  
(define (turtle-return item)
  (lambda (turtle-state) 
    (doublet item turtle-state)))

(define (turtle-bind mv mf)
  (lambda (turtle-state)
    (match (mv turtle-state)
      [(doublet val new-state)
       (let ((new-mv (mf val)))
         (new-mv new-state))])))

(define (turtle-plus mv1 mv2)
  (lambda (turtle-state)
    (match (mv1 turtle-state)
      [(doublet val1 new-state)
       (mv2 new-state)])))

(define m-turtle
  (list 
   (cons 'bind turtle-bind)
   (cons 'return turtle-return)
   (cons 'plus turtle-plus)
   (cons 'zero #f)))

(define-syntax (turtle-let* stx)
  (syntax-case stx ()
    [(turtle-let* ((var expr)) body ...)
     (syntax (turtle-bind expr (lambda (var)
                                 body ...)))]
    [(turtle-let* ((var0 expr0) (var expr) ...) body ...)
     (syntax
      (turtle-bind expr0 
                   (lambda (var0)
                     (turtle-let* ((var expr) ...) body ...))))]))

(define (get-val symbol . args)
  (match args
    [(list) 
     (lambda (st)
       (doublet (dict-ref st symbol) st))]
    [(list default)
     (lambda (st)
       (doublet (dict-ref st symbol 
                          (lambda () default)) st))]))

(define (set-val symbol val)
  (lambda (st)
    (doublet val (dict-set st symbol val))))

(define (add-line-between p1 p2)
  (turtle-let* 
   ((things (get-val 'things-to-draw '()))
    (things (set-val 'things-to-draw (cons (line-segment p1 p2) things))))
   (turtle-return things)))

(define (jump-to x y)
  (set-val 'pos (point x y)))

(define (raw-move amount)
  (turtle-let*
   ((cur (get-val 'pos (point 150 150)))
    (dir (get-val 'facing 0))
    (new-pos (set-val 'pos (point+ cur (point-scale (radians->point-vector dir) amount)))))
   (turtle-return new-pos)))

(define (move amount)
  (turtle-let* 
   ((p1 (get-val 'pos (point 150 150)))
    (p2 (raw-move amount))
    (move-fun (get-val 'motion-function))
    (_ (move-fun p1 p2)))
   (turtle-return p2)))

(define (turn amount)
  (turtle-let* 
   ((a (get-val 'facing 0)))
   (set-val 'facing (+ a amount))))

(define (make-turtle)
  (list
   (cons 'pos (point 150 150))
   (cons 'facing 0)
   (cons 'things-to-draw '())
   (cons 'motion-function add-line-between)))

(define (set-motion-function to)
  (set-val 'motion-function to))

(define pen-up
  (set-val 'motion-function 
           (lambda (p1 p2)
             (turtle-return p2))))

(define pen-down 
  (set-val 'motion-function add-line-between))

(define turtle-frame (new frame% [label "Turtle"]
                          [width 300]
                          [height 300]))
(send turtle-frame show #f)
(define (turtle-paint-callback self dc) 'pass)
(define turtle-canvas (new canvas% 
                           [parent turtle-frame]
                           [paint-callback (lambda (self dc)
                                             (turtle-paint-callback self dc))]))

(define (turtle-show fun . args)
  (dlet1 
   ((:> or 
        '((width . 300)
          (height . 300)))
    width 'width
    height 'height) args
   (let* ((final-state (fun (make-turtle)))
          (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
     (send turtle-frame show #f)
     (set! turtle-paint-callback
           (lambda (self dc)
             (send dc set-background "white")
             (send dc clear)
             (let loop ((things things))
               (if (empty? things) 'done
                   (begin (draw-shape dc (car things))
                          (loop (cdr things)))))))
     (send turtle-frame show #t))))
  
(define (turtle->pict fun . args)
  (dlet1 ((:> or 
              '((width . 300)
                (height . 300)))
          width 'width
          height 'height) 
         args
         (let* ((bitmap (make-object bitmap% width
                          height
                          #f
                          #f))
                (dc (new bitmap-dc% [bitmap bitmap]))
                (final-state (fun (make-turtle)))
                (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
    (let loop ((things things))
      (match things
        [(list) (bitmap->pict bitmap)]
        [(cons thing things)
         (draw-shape dc thing)
         (loop things)])))))
  
(define (turtle->png fun . args)
  (dlet1 ((:> or 
              '((width . 300)
                (height . 300)
                (filename . "/tmp/turtle.png")))
          width 'width
          height 'height
          filename 'filename)
         args
         (let* ((bitmap (make-object bitmap% width
                          height
                          #f
                          #f))
                (dc (new bitmap-dc% [bitmap bitmap]))
                (final-state (fun (make-turtle)))
                (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
    (let loop ((things things))
      (match things
        [(list) (send bitmap save-file filename 'png)]
        [(cons thing things)
         (draw-shape dc thing)
         (loop things)])))))

(define (n-times n tf)
  (match n 
    [0 (turtle-return #t)]
    [1 tf]
    [(? positive? (? integer?))
     (turtle-let* ((_ tf))
                  (n-times (- n 1) tf))]))

(provide make-turtle
         turn 
         move 
         get-val
         set-val
         add-line-between
         m-turtle
         turtle-return
         turtle-bind
         turtle-plus
         jump-to
         pen-up
         turtle-show
         turtle->pict
         turtle->png
         turtle-let*
         n-times
         pen-down)