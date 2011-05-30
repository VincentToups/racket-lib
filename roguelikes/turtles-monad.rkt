#lang racket

(require functional/monads
         (except-in functional/point-free compose)
         (rename-in functional/point-free [compose point-free.compose])
         utilities/fancy-destructuring
         utilities/randomness
         utilities/dont-do
         (except-in utilities/lists any)
         racket/match
         racket/dict
         racket/gui)
         
(define (pair a b)
  (cons a b))

(struct doublet (a b))

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

(define m-turtles
  (list (cons 'bind m-bind)
        (cons 'return m-return)
        (cons 'plus #f)
        (cons 'zero (lambda (dblt) (pair '() (cdr dblt))))))

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

(define<turtles> (dipl lo g ^ symbol fun)
  (let ((new-val (fun (dict-ref lo symbol #f))))
    (doublet (list (doublet new-val
                            (dict-set lo symbol new-val)))
             g)))

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

(struct point (x y))
(struct line (p1 p2))

(define init-state 
  (doublet '() '()))


(define (jump-to x y)
  (mlet* m-turtles 
         ((_ (setl 'pos (point x y))))
         (m-return _)))

(define (direction->vector dir)
  (point (cos dir) 
         (sin dir)))

(define (scale pt amt)
  (match pt
    [(point x y) (point (* x amt) (* y amt))]))

(define (point+ p1 p2)
  (match-let 
   ([(point x1 y1) p1]
    [(point x2 y2) p2])
   (point 
    (+ x1 x2)
    (+ y1 y2))))

(define (point- p1 p2)
  (match-let 
   ([(point x1 y1) p1]
    [(point x2 y2) p2])
   (point 
    (- x1 x2)
    (- y1 y2))))

(define (point-len pt)
  (match pt
    [(point x y) (sqrt (+ (* x x) (* y y)))]))

(define (point-dist p1 p2)
  (point-len (point- p1 p2)))


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
          (lines (setg 'draw-these (cons (line 
                                          (point x1 y1)
                                          (point x2 y2))
                                         (if lines lines '())))))
         (m-return lines)))

(define (add-line-pts p1 p2)
  (mlet* m-turtles 
         ((lines (getg-or 'draw-these '()))
          (lines (setg 'draw-these (cons (line 
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

(define (draw-line dc a-line)
  (match-let* ([(line p1 p2) a-line]
               [(point x1 y1) p1]
               [(point x2 y2) p2])
              (send dc draw-line x1 y1 x2 y2)))

(define (n-gon n edge-len)
  (let* ((int-ang (/ (* (- n 2) pi) n))
         (ext-ang (- pi int-ang)))
    (n-times
     (mlet* m-turtles
            ((p (move-line edge-len))
             (a (turn ext-ang)))
            (m-return p))
     n)))

(struct circle (c r))

(define (draw-circle dc a-circle)
  (match-let* ([(circle c r) a-circle]
               [(point x y) c])
              (send dc draw-arc x y r r 0 (* 2 pi))))

(define (draw-thing dc thing)
  (cond
   ((line? thing)
    (draw-line dc thing))
   ((circle? thing)
    (draw-circle dc thing))
   ((procedure? thing)
    (thing dc))
   (else 'pass)))

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
  (for-each (>partial draw-thing dc) (send (send self get-parent) get-things-to-draw )))

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




(define frame (new turtle-frame%
                   [label "Turtles!"]
                   [width 300]
                   [height 300]
                   [things-to-draw '()]))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback draw-current-drawing]))


(send frame show #t)

(send frame set-things-to-draw!
      (list
       (line (point 500 250)
             (point 500 0))))
(send canvas refresh-now)

(define (turtles-go
         turtle-f)
  (let* ((done (turtle-f init-state))
         (gstate (doublet-b done))
         (things-to-draw (dict-ref gstate 'draw-these '())))
    (send frame set-things-to-draw! things-to-draw)
    (send frame show #t)
    (send canvas refresh-now)))

(turtles-go
 (mlet*
  m-turtles
  ((_ (jump-to 150 150))
   (_ (split-setl 'move*-fun (list add-line-pts add-circle-pts)))
   (_ (n-times (mlet* m-turtles
                      ((p (jitter-by 1 4))
                       (h (split-setl 'helicity '(-1 1)))
                       (r (turn (/ pi 4)))
                       (ln (move* 20))
                       (r (turn (- (/ pi 8))))
                       (ln (move* 15)))
                      (m-return ln)) 2)))
  (m-return #t)))

(dont-do
(circle-r (car (dict-ref (doublet-b ((mlet*
  m-turtles
  ((_ (jump-to 150 150))
   (_ (split-setl 'move*-fun (list add-line-pts add-circle-pts)))
   (_ (n-times (mlet* m-turtles
                      ((p (jitter-by 1 4))
                       (h (split-setl 'helicity '(-1 1)))
                       (r (turn (/ pi 4)))
                       (ln (move* 20))
                       (r (turn (- (/ pi 8))))
                       (ln (move* 15)))
                      (m-return ln)) 2)))
  (m-return #t)) init-state)) 'draw-these)))

(turtles-go
 (mlet* m-turtles
        ((p (jump-to 150 150))
         (p2 (n-gon 6 30)))
        (m-return p2)))

(turtles-go
 (mlet* m-turtles
        ((p (split-setl
             'pos
             (list (point 150 150)
                   (point 200 150)
                   (point 150 200)
                   (point 200 200))))
             (p2 (n-gon 6 30)))
            (m-return p2)))


((move 20) init-state)

((mlet* m-turtles
        ((_ (mlet* m-turtles
                   ((_ (split-setl 'x '(1 2 3)))
             (x (getl 'x)))
            (m-return x)))
  (x (getl 'x)))
 (m-return x)) init-state)


((mlet* m-turtles
       ((f (getl-or 'f 
       (m-return f)) init-state)
        ))))
