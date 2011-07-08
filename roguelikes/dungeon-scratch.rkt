#lang racket
(require (except-in utilities/lists any all)
         (rename-in utilities/lists [any lists.any] [all lists.all])
         functional/monads
         (except-in functional/point-free compose)
         (rename-in functional/point-free [compose comp])
         utilities/one-of
         utilities/simple-infix
         utilities/planar-geometry
         utilities/draw-planar-geometry
         utilities/fancy-destructuring
         roguelikes/turtles-monad
         racket/base
         racket/dict
         racket/match)

(define (main)
  (print "Hello World"))

(define turtles-retire turtles-zero)

(define (add-room-centered x y w h)
  (let ((w (force w))
        (h (force h)))
    (mlet* m-turtles
           ((rooms (getg-or 'rooms '()))
            (non-monadically:
             ((room (rectangle
                     (point (round (- x (/ w 2)))
                            (round (- y (/ h 2))))
                     (point (round (+ x (/ w 2)))
                            (round (+ y (/ h 2))))))))
            (rooms (setg 'rooms
                         (cons
                          room
                          rooms))))
           (m-return room))))

(define (add-hall-between . args)
  (match args
    ((list p1 p2)
     (mlet* m-turtles
            ((halls (getg-or 'halls '()))
             (non-monadically:
              ((hall (line-segment p1 p2))))
             (halls (setg 'halls
                          (cons
                           hall
                           halls))))
            (m-return hall)))
    ((list x1 y1 x2 y2)
     (add-hall-between (point x1 y1)
                       (point x2 y2)))))

(define (move-add-hall amt)
  (mlet* m-turtles
         ((pos (getl-or 'pos (point 0 0)))
          (pos2 (move amt)))
         (add-hall-between pos pos2)))

(define (room-for-room? room rooms)
  (let loop ((rooms rooms))
    (cond ((empty? rooms) #t)
          ((rectangles-overlap? room (car rooms)) #f)
          (else
           (loop (cdr rooms))))))

(define (undelimited-string-of-features list)
  (let loop ((list list)
             (acc ""))
    (cond ((empty? list) acc)
          (else
           (loop (cdr list)
                 (string-append acc (format "~a" (feature->string (car list)))
                                (if (empty? (cdr list)) "" " ")))))))

(define (room-in-range? r rectangle)
  (let loop ((corners (rectangle->corners r)))
    (cond ((empty? corners) #t)
          ((not (point-in-rectangle? (car corners) rectangle)) #f)
          (else (loop (cdr corners))))))

(define (try-to-add-room-centered x y w h)
  (let ((room (rectangle
               (point (round (- x (/ w 2)))
                      (round (- y (/ h 2))))
               (point (round (+ x (/ w 2)))
                      (round (+ y (/ h 2)))))))
    (mlet* m-turtles
           ((rooms (getg-or 'rooms '())))
                                        ;(display (format "~n(room-for-room?~n ~a~n (list ~a))~n" (feature->string room) (undelimited-string-of-features rooms)))
           (if (and (room-for-room? room rooms)
                    (room-in-range? room (rectangle (point 0 0) (point 300 300))))
               (begin
                 (setg 'rooms
                       (cons room rooms)))
               (begin
                 (m-return #f))))))

(define (add-room-here w h)
  (mlet* m-turtles
         ((pos (getl-or 'pos (point 150 150)))
          (room (add-room-centered (point-x pos) (point-y pos) w h)))
         (m-return room)))

(define (point-in-a-room? pt rooms)
  (cond ((empty? rooms) #f
         (let ((room (car rooms))
               (rooms (cdr rooms)))
           (if (point-in-rectangle? pt room) #t
               (point-in-a-room? pt rooms))))))

(define (try-to-add-room/hall . args)
  (dlet1 ((:> or '((dead-end-p . 0.1)
                   (new-connection-p . 0.4)
                   (room-width . 6)
                   (room-height . 6)
                   (hall-length . 10)))
          hall-length 'hall-length
          room-width 'room-width
          room-height 'room-height
          dead-end-p 'dead-end-p
          new-connection-p 'new-connection-p) args
          (mlet* m-turtles
                 ((old-pos (getl 'pos))
                  (new-pos (move hall-length))
                  (rooms (getg-or  'rooms '()))
                  (success? (try-to-add-room-centered (point-x new-pos)
                                                      (point-y new-pos)
                                                      room-width room-height)))
                 (cond
                  ((or success?
                       (and (point-in-a-room? new-pos rooms)
                            ($ (random) < new-connection-p))
                       (and (point-in-rectangle? new-pos (rectangle (point 0 0) (point 300 300)))
                            ($ (random) < dead-end-p)))
                   (mlet* m-turtles
                          ((h (add-hall-between old-pos new-pos))
                           (p (setl 'pos new-pos)))
                          (m-return h)))
                  (else (mlet* m-turtles
                               ((pos (setl 'pos old-pos)))
                               (m-return #f)))))))

(define (stochastic-turn)
  (one-of
   (1 (turn (/ pi 2)))
   (1 (turn (- (/ pi 2))))))

(define (maybe-bifurcate)
  (one-of
   (1 helicity-split)
   (1 pass)))

(define (maybe-retire with-probability)
  (if ($ (random) < with-probability)
      turtles-retire
      pass))

(define render-dungeon-simple
  (mlet* m-turtles
         ((rooms (getg-or 'rooms '()))
          (halls (getg-or 'halls '())))
         (setg 'draw-these (append rooms halls))))

(define (maybe-turn with-probability amount)
  (if ($ (random) < with-probability)
      (turn amount)
      pass))

(define clamp-facing
  (mlet* m-turtles
         ((dir (getl-or 'facing (/ pi 2))))
         (setl 'facing
               (point-vector->radians (round-point (radians->point-vector dir))))))

(define (step)
  (mlet* m-turtles
         (
          (u (maybe-turn 0.5 (/ pi 2)))
          (r (try-to-add-room/hall '(hall-length . 16) '(new-connection-p . 0.3)))
          (f clamp-facing)
          (r (maybe-retire 0.00)))
         (m-return 'step)))

(define (random-room-dim)
  (+ 8 (* 2 (random 5))))

(define (random-hall-length)
  (+ 12 (* 2 (random 5))))

(define (step*)
  (mlet* m-turtles
         (
          (u (maybe-turn 0.5 (/ pi 2)))
          (r (try-to-add-room/hall
              '(new-connection-p . 0.3)
              (cons 'room-width (random-room-dim))
              (cons 'room-height (random-room-dim))
              (cons 'hall-length (random-hall-length))
              ))
          (f clamp-facing)
          (r (maybe-retire 0.00)))
         (m-return 'step)))


(turtles-go
 (mlet* m-turtles ((stub (setl 'pos (point 150 150)))
                   (room (add-room-here 6 6))
                   (_ helicity-split)
                   (_ (n-times-call step* 64))
                   (r render-dungeon-simple))
        (m-return #t)))

(turtles-go->svg
 (mlet* m-turtles ((stub (setl 'pos (point 150 150)))
                   (room (add-room-here 6 6))
                   (_ helicity-split)
                   (_ (n-times-call step* 64))
                   (r render-dungeon-simple))
        (m-return #t)))


(provide main)




