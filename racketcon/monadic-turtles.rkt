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

(define (turtles-return item)
  (lambda (state-doublet)
    (match state-doublet
      [(doublet local-state global-state)
       (doublet (list (doublet item local-state)) global-state)])))

(define (reduce proc lst)
  (foldl (lambda (it ac)
           (proc ac it))
         (car lst)
         (cdr lst)))

(define (turtles-bind turtlesf turtlesf-prod)
  (lambda (state-doublet)
    (match (turtlesf state-doublet)
      [(doublet local-doublets global-state)
       (let loop ((local-doublets local-doublets)
                  (global-state global-state)
                  (local-doublets-out '()))
         (match local-doublets
           [(list) (doublet (reduce append (reverse local-doublets-out)) global-state)]
           [(cons local-doublet local-doublets)
            (match local-doublet
              [(doublet val local-state)
               (let ((new-turtle-fun (turtlesf-prod val)))
                 (match (new-turtle-fun (doublet local-state global-state))
                   [(doublet sub-local-doublets
                             global-state)
                    (loop local-doublets global-state (cons sub-local-doublets local-doublets-out))]))])]))])))

(define-syntax (turtles-let* stx)
  (syntax-case stx ()
    [(turtles-let* ((var expr)) body ...)
     (syntax (turtles-bind expr (lambda (var) body ...)))]
    [(turtles-let* ((var0 expr0) (var expr) ...) body ...)
     (syntax (turtles-bind expr0 (lambda (var0)
                           (turtles-let* ((var expr) ...) body ...))))]))

(define (turtles-plus tf1 tf2)
  (turtles-let* 
   ((_ tf1))
   tf2))
    
(define turtles-zero 
  (lambda (state-doublet)
    (doublet (list) (doublet-b state-doublet))))

(define m-turtles 
  (list 
   (cons 'bind turtles-bind)
   (cons 'return turtles-return)
   (cons 'plus turtles-plus)
   (cons 'zero turtles-zero)))

(define (set-local symbol val)
  (lambda (state-doublet)
    (match state-doublet
      [(doublet local-state global-state)
       (doublet (list 
                 (doublet val
                          (dict-set local-state symbol val)))
                global-state)])))

(define (get-local symbol . args)
  (match args 
    [(list) (lambda (state-doublet)
              (match state-doublet
                [(doublet local-state global-state)
                 (doublet (list (doublet (dict-ref local-state symbol) local-state))
                          global-state)]))]
    [(list or-val)
     (lambda (state-doublet)
       (match state-doublet
         [(doublet local-state global-state)
          (doublet (list (doublet (dict-ref local-state symbol (lambda () or-val)) local-state))
                   global-state)]))]))

(define (get-global symbol . args)
  (match args
    [(list) 
     (lambda (state-doublet)
       (match state-doublet
         [(doublet local-state global-state)
          (doublet (list (doublet (dict-ref global-state symbol) local-state))
                   global-state)]))]
    [(list or-val)
     (lambda (state-doublet)
       (match state-doublet 
         [(doublet local-state global-state)
          (doublet (list (doublet (dict-ref global-state symbol (lambda () or-val)) local-state))
                   global-state)]))]))
       
(define (set-global symbol val)
  (lambda (state-doublet)
    (match state-doublet
      [(doublet local-state global-state)
       (doublet (list
                 (doublet val local-state))
                (dict-set global-state symbol val))])))

(define (set-simultaneously . associations)
  (lambda (state-doublet)
    (match state-doublet
      [(doublet local-state global-state)
       (let loop ((associations associations)
                  (acc '()))
         (match associations 
           [(list) (doublet (reverse acc) global-state)]
           [(cons association associations)
            (loop associations
                  (cons (doublet
                         (map cadr association)
                         (foldl
                          (lambda (pair local-state)
                            (dict-set local-state (car pair) (cadr pair)))
                          local-state
                          association))
                        acc))]))])))

(define (split-set symbol . vals)
  (lambda (state-doublet)
    (match state-doublet
      [(doublet local-state global-state)
       (doublet (map
                 (lambda (val)
                   (doublet val (dict-set local-state symbol val)))
                 vals)
                global-state)])))

(define (prep-jump-args lst)
  (let loop [(lst lst)
             (acc '())]
    (match lst
      [(list a b) (reverse (cons (point a b) acc))]
      [(cons a (cons b rest))
       (loop rest (cons (point a b) acc))])))

(define (jump-to . args)
  (let [(locations (prep-jump-args args))]
    (apply split-set 'pos locations)))

(define (turn . args)
  (turtles-let* 
   ((facing (get-local 'facing 0))
    (helicity (get-local 'helicity 1))
    (facing (apply split-set 'facing 
                       (map (lambda (x)
                              (+ (* x helicity) facing))
                            args))))
   (turtles-return facing)))

(define get-facing-vector
  (turtles-let*
   ((fac (get-local 'facing 0)))
   (turtles-return (radians->point-vector fac))))

(define (move-raw . args)
  (turtles-let*
   ((pos (get-local 'pos (point 150 150)))
    (scaling (get-local 'scaling 1))
    (facv get-facing-vector))
   (apply split-set 'pos
          (map
           (lambda (amount)
             (point+
              pos
              (point-scale facv (* scaling amount))))
           args))))

(define (move . args)
  (turtles-let*
   ((p1 (get-local 'pos (point 150 150)))
    (p2 (apply move-raw args))
    (draw-fun (get-local 'motion-function add-line-pts)))
   (draw-fun p1 p2)))

(define (add-line-pts p1 p2)
  (turtles-let* 
   ((things-to-draw (get-global 'things-to-draw))
    (things-to-draw (set-global 'things-to-draw
                                (cons (line-segment p1 p2) things-to-draw))))
   (turtles-return things-to-draw)))


(define (make-turtles)
  (doublet
   ; local state
   (list (cons 'pos (point 150 150))
         (cons 'facing 0)
         (cons 'helicity 1)
         (cons 'motion-function add-line-pts)
         (cons 'scaling 1))
   ; global state
   (list (cons 'things-to-draw '()))))

(define (n-times n tsf)
  (match n 
    [0 (turtles-return #t)]
    [1 tsf]
    [(? positive? (? integer?))
     (turtles-let* ((_ tsf))
                  (n-times (- n 1) tsf))]))

(define (turtles->pict fun . args)
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
                (final-state (fun (make-turtles)))
                (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
    (let loop ((things things))
      (match things
        [(list) (bitmap->pict bitmap)]
        [(cons thing things)
         (draw-shape dc thing)
         (loop things)])))))

(define turtle-frame (new frame% [label "Turtle"]
                          [width 300]
                          [height 300]))
(send turtle-frame show #f)
(define (turtle-paint-callback self dc) 'pass)
(define turtle-canvas (new canvas% 
                           [parent turtle-frame]
                           [paint-callback (lambda (self dc)
                                             (turtle-paint-callback self dc))]))

(define (turtles-show fun . args)
  (dlet1 
   ((:> or 
        '((width . 300)
          (height . 300)))
    width 'width
    height 'height) args
   (let* ((final-state (fun (make-turtles)))
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

(define (turtles->png fun . args)
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
                (final-state (fun (make-turtles)))
                (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
    (let loop ((things things))
      (match things
        [(list) (send bitmap save-file filename 'png)]
        [(cons thing things)
         (draw-shape dc thing)
         (loop things)])))))

(define (set-motion-function . args)
  (apply split-set 'motion-function args))

(provide set-motion-function
         turtles->png
         turtles->pict
         turtles-show
         n-times
         turtles-return
         turtles-bind
         turtles-plus
         turtles-zero
         move
         turn
         set-local
         set-global
         get-local
         get-global
         split-set
         get-facing-vector
         set-simultaneously
         jump-to
         m-turtles
         turtles-let*
         doublet
         doublet-a
         doublet-b
         doublet?
         add-line-pts)
         