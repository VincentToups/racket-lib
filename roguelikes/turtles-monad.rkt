#lang racket

(require functional/monads
         (except-in functional/point-free compose)
         (rename-in functional/point-free [compose point-free.compose])
         utilities/fancy-destructuring
         utilities/randomness
         utilities/dont-do
         utilities/planar-geometry
         utilities/draw-planar-geometry
         (except-in utilities/lists any)
         racket/match
         racket/dict
         (rename-in (only-in slideshow/pict bitmap) [bitmap bitmap->pict])
         racket/gui)
         

(struct doublet (a b)
        #:property
        prop:custom-write
        (lambda (s port mode)
          (display "(doublet " port)
          (display (doublet-a s) port)
          (display (doublet-b s) port)))

;;; A monadic value here is a function which takes a 
;;; state-doublet (local-state global-state) and returns 
;;; a pair of whose car is a list of value/local-state 
;;; pairs and whose cadr is a a global state.>
;;; Hence, return is:

(define (m-return val)
  (lambda (st-doublet)
    (doublet
     (list (doublet val (doublet-a st-doublet)))
     (doublet-b st-doublet))))



(define (m-bind mv mf)
  (lambda (st-dblt)
    (match (mv st-dblt)
      [(doublet pairs gstate) 
       (let loop ((pairs pairs)
                  (gstate gstate)
                  (pairs-acc '()))
         (cond
          ((empty? pairs)
           (doublet pairs-acc 
                    gstate))
          (else
                                        ;(displayln (format "Inside bind hd is ~a" (car pairs)))
                                        ;(displayln (format "Inside bind local-state is ~a" (doublet-b (car pairs))))
                                        ;(displayln (format "Inside bind input to a mv is (doublet ~a ~a)." (doublet-a (car pairs))
                                        ;                   (doublet-b (car pairs))))
           (let* ((hd (car pairs))
                  (tl (cdr pairs))
                  (inner-r ((mf (doublet-a hd)) (doublet (doublet-b hd) gstate)))
                  (inner-pairs (doublet-a inner-r))
                  (new-gstate (doublet-b inner-r)))
             (loop tl new-gstate (append pairs-acc inner-pairs))))))])))



(define-syntax (define<turtles> stx)
  (syntax-case stx (^)
    [(_ (id lstate-id gstate-id) body ...)
     (syntax (define (id dblt)
               (match dblt
                 [(doublet lstate-id gstate-id) 
                  (begin body ...)])))]
    [(_ (id lstate-id gstate-id ^ var) body ...)
     (syntax (define (id var)
               (lambda (dblt)
                 (match dblt
                   [(doublet lstate-id gstate-id)
                    (begin body ...)]))))]
    [(_ (id lstate-id gstate-id ^ var ...) body ...)
     (syntax (define (id var ...)
               (lambda (dblt)
                 (match dblt
                   [(doublet lstate-id gstate-id)
                    (begin body ...)]))))]
    [(_ id val)
     (syntax (lambda (dblt)
               (match dblt
                 [(doublet lstate-id gstate-id) 
                  (doublet (list (doublet val lstate-id)) gstate-id)])))]))

(define<turtles> (setg lo g ^ symbol value)
  (doublet (list (doublet value lo))
           (dict-set g symbol value)))

(define<turtles> (dipg lo g ^ symbol fun)
  (let ((new-val (fun (dict-ref g symbol #f))))
    (doublet (list (doublet new-val lo))
             (dict-set g symbol new-val))))

(define<turtles> (getg lo g ^ symbol)
  (doublet (list (doublet (dict-ref g symbol #f) lo))
           g))

(define<turtles> (getg-or lo g ^ symbol or-val)
  (doublet (list (doublet (dict-ref g symbol or-val) lo))
           g))

(define<turtles> (setl lo g ^ symbol val)
                                        ;(displayln (format "Inside setl lo is: ~a" lo))
  (doublet (list (doublet val
                          (dict-set lo symbol val)))
           g))

(define<turtles> (turtles-zero lo gl)
  (doublet '() gl))

(define<turtles> (turtles-split2 lo gl ^ tf1 tf2)
  (let* ((r1 (tf1 (doublet lo gl)))
         (new-gl (doublet-b r1))
         (r2 (tf2 (doublet lo new-gl))))
    (doublet
     (append
      (doublet-a r1)
      (doublet-a r2))
     (doublet-b r2))))

(define<turtles> (dipl lo g ^ symbol fun)
  (let ((new-val (fun (dict-ref lo symbol #f))))
    (doublet (list (doublet new-val
                            (dict-set lo symbol new-val)))
             g)))

(define m-turtles
  (list (cons 'bind m-bind)
        (cons 'return m-return)
        (cons 'plus #f)
        (cons 'zero turtles-zero)))

(define turtles-return (dict-ref m-turtles 'return))

(define (getl* sym)
  (lambda (a-doublet)
                                        ;(displayln (format "getl* doublet: ~a" a-doublet))
    (match a-doublet
      [(doublet lo g)
       (doublet
        (list (doublet (dict-ref lo sym #f) lo))
        g)])))

(define<turtles> (getl lo g ^ symbol)
                                        ;(displayln (format "Inside getl lo is: ~a" lo))
  (doublet (list (doublet (dict-ref lo symbol #f) lo))
           g))

(define<turtles> (getl-or lo g ^ symbol or-val)
  (doublet (list (doublet (dict-ref lo symbol or-val) lo))
           g))

(define<turtles> (split-setl lo g ^ symbol vals)
  (doublet 
   (map (lambda (val)
          (doublet val (dict-set lo symbol val)))
        vals)
   g))

(define<turtles> (pass lo gl)
  (doublet (list (doublet 'pass lo)) gl))



;; (struct point (x y))
;; (struct line (p1 p2))

(define init-state 
  (doublet '() '()))


(define (jump-to x y)
  (mlet* m-turtles 
         ((_ (setl 'pos (point x y))))
         (m-return _)))

(define (direction->vector dir)
  (point (cos dir) 
         (sin dir)))

(define scale point-scale)

(define (move amt)
  (mlet* m-turtles
         ((pos (getl-or 'pos (point 0 0)))
          (facing (getl-or 'facing (/ pi 2)))
          (non-monadically: 
           ((facing-vector 
             (direction->vector facing))))
          (new-pos (setl 'pos (point+ 
                               pos (scale facing-vector amt)))))
         (m-return new-pos)))

(define (move-line amt)
  (mlet* m-turtles
         ((pos (getl-or 'pos (point 0 0)))
          (new-pos (move amt)))
         (add-line-pts pos new-pos)))


(define (face-in direction)
  (mlet* m-turtles
         ((_ (setl 'facing direction)))
         (m-return _)))

(define (set-helicity h)
  (mlet* m-turtles
         ((_ (setl 'helicity h)))
         (m-return _)))

(define (turn amount)
  (mlet* m-turtles
         ((helicity (getl-or 'helicity 1))
          (facing (getl-or 'facing (/ pi 2)))
          (new-facing (setl 'facing
                            (+ (* amount helicity) facing))))
         (m-return new-facing)))

(define helicity-split
  (mlet* m-turtles
         ((_ (split-setl 'helicity (list -1 1))))
         (m-return _)))


(define (add-line x1 y1 x2 y2)
  (mlet* m-turtles 
         ((lines (getg 'draw-these))
          (lines (setg 'draw-these (cons (line-segment 
                                          (point x1 y1)
                                          (point x2 y2))
                                         (if lines lines '())))))
         (m-return lines)))

(define (add-line-pts p1 p2)
  (mlet* m-turtles 
         ((lines (getg-or 'draw-these '()))
          (lines (setg 'draw-these (cons (line-segment 
                                          p1
                                          p2)
                                         lines))))
         (m-return lines)))

(define (add-circle-pts p1 p2)
  (mlet* m-turtles
         ((things (getg-or 'draw-these '()))
          (things (setg 'draw-these
                        (cons
                         (circle p1
                                 (point-dist p1 p2))
                         things))))
         (m-return things)))

(define (n-times fun n)
  (if (= n 1) fun
      (mlet* m-turtles
             ((_ fun))
             (n-times fun (- n 1)))))

(define (n-times-call fun n)
  (if (= n 1) (fun)
      (mlet* m-turtles
             ((_ (fun)))
             (n-times-call fun (- n 1)))))


(define (n-gon n edge-len)
  (let* ((int-ang (/ (* (- n 2) pi) n))
         (ext-ang (- pi int-ang)))
    (n-times
     (mlet* m-turtles
            ((p (move-line edge-len))
             (a (turn ext-ang)))
            (m-return p))
     n)))

;; (struct circle (c r))


(define (jitter-by mag n)
  (mlet* m-turtles
         ((pos (getl-or 'pos (point 0 0)))
          (pos (split-setl 'pos
                           (map
                            (lambda (_)
                              (point+ pos
                                      (point (random mag)
                                             (random mag))))
                            (range n)))))
         (m-return pos)))


(define *current-drawing* '())

(define (draw-current-drawing self dc)
  (for-each (>partial draw-shape dc) (send (send self get-parent) get-things-to-draw )))

(define turtle-frame%
  (class frame%
    (init-field (things-to-draw '()))
    (define/public (set-things-to-draw! new-value)
      (set! things-to-draw new-value))
    (define/public (get-things-to-draw)
      things-to-draw)
    (super-instantiate ())))

(define (move* amt)
  (mlet* m-turtles 
         (
          (jitter-mag (getl-or 'jitter-mag 0))
          (non-monadically:
           ((amt (+ amt (random-normal 0 jitter-mag)))))
          (pos (getl-or 'pos (point 0 0)))
          (_ (move amt))
          (pos2 (getl 'pos))
          (f (getl-or 'move*-fun (lambda () add-line-pts)))
          (_ (f pos pos2)))
         (m-return pos2)))




                                        ;(define frame (new turtle-frame%
                                        ;                   [label "Turtles!"]
                                        ;                   [width 300]
                                        ;                   [height 300]
                                        ;                   [things-to-draw '()]))
                                        ;
                                        ;(define canvas (new canvas%
                                        ;                    [parent frame]
                                        ;                    [paint-callback draw-current-drawing]))
                                        ;
                                        ;
                                        ;(send frame show #t)
                                        ;
                                        ;(send frame set-things-to-draw!
                                        ;      (list
                                        ;       (line-segment (point 500 250)
                                        ;                     (point 500 0))))
                                        ;(send canvas refresh-now)

(define turtles-go-frame #f)
(define (turtles-go
         turtle-f . optional-arg-dict)
  (dlet1 ((:> or 
              '((width . 300)
                (height . 300)
                (label . "I Like Turtles!")))
          width 'width
          height 'height
          label 'label) optional-arg-dict
          (let* ((done (turtle-f init-state))
                 (gstate (doublet-b done))
                 (things-to-draw (dict-ref gstate 'draw-these '()))
                 (frame (new turtle-frame%
                             [label label]
                             [width width]
                             [height height]
                             [things-to-draw '()]))
                 (canvas (new canvas%
                              [parent frame]
                              [paint-callback draw-current-drawing])))
            (if turtles-go-frame (send turtles-go-frame show #f) #f)
            (send frame set-things-to-draw! things-to-draw)
            (send frame show #t)
            (send canvas refresh-now)
            (set! turtles-go-frame frame))))

(define (draw-things dc things)
  (for-each (>partial draw-shape dc) things))

(define (turtles-go->svg turtle-f . optional-arg-dict)
  (dlet1 ((:> or 
              '((write-to . "/tmp/turtle.svg")
                (width . 300)
                (height . 300)))
          width 'width
          height 'height
          write-to 'write-to) optional-arg-dict
          (let* ((done (turtle-f init-state))
                 (gstate (doublet-b done))
                 (things-to-draw (dict-ref gstate 'draw-these '()))
                 (svg (new svg-dc% [width width]
                           [height height]
                           [output write-to]
                           [exists 'replace])))
            (send svg start-doc "Turtle!")
            (send svg start-page)
            (draw-things svg things-to-draw)
            (send svg end-page)
            (send svg end-doc))))


(define (draw-self . args)
  (match args
    [(list)
     (draw-self 4)]
    [(list scale)
     (mlet* m-turtles
            ((p (getl 'pos))
             (r (getl 'facing))
             (np (move scale))
             (c (add-circle-pts p np))
             (np (move scale))
             (l (add-line-pts p np))
             (p (setl 'pos p)))
            (m-return p))]))

(define (turtles-go->bitmap turtle-f . optional-arg-dict)
  (dlet1 ((:> or 
              '((also-save-as . #f)
                (extension . png)
                (width . 300)
                (height . 300)))
          width 'width
          height 'height
          also-save-as 'also-save-as
          extension 'extension
          ) optional-arg-dict
          (let* ((done (turtle-f init-state))
                 (gstate (doublet-b done))
                 (things-to-draw (dict-ref gstate 'draw-these '()))
                 (bitmap (make-object bitmap% width height #f #f))
                 (bitmap-dc (new bitmap-dc% [bitmap bitmap])))
            (send bitmap-dc erase)
            (send bitmap-dc set-background "white")
            (send bitmap-dc clear)
            (draw-things bitmap-dc things-to-draw)
            (if also-save-as
                (send bitmap save-file also-save-as extension)
                #f)
            bitmap)))

(define (turtles-go->pict turtle-f . optional-arg-dict)
  (let ((bitmap (apply turtles-go->bitmap turtle-f optional-arg-dict)))
    (bitmap->pict bitmap)))


(provide
 m-turtles
 define<turtles>
 turtles-go->pict
 turtles-go->bitmap
 draw-self
 init-state
 setg
 getg
 getg-or
 setl
 getl
 getl-or
 split-setl
 dipl
 dipg
 jump-to
 direction->vector
 move
 move-line
 move*
 face-in
 set-helicity
 turn
 helicity-split
 draw-things
 turtles-go->svg
 add-line
 add-line-pts
 add-circle-pts
 n-times
 n-gon
 jitter-by
 turtles-go
 pass
 doublet-a
 doublet-b
 doublet
 doublet?
 turtles-zero
 turtles-return
 n-times-call
 (all-from-out utilities/planar-geometry)
 (all-from-out utilities/draw-planar-geometry))

 