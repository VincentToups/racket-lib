#lang racket

(require racket/match
         functional/point-free
         functional/monads
         utilities/lists)

(struct point (x y))
(struct room (x y w h))
(struct hall-segment (p1 p2))

(define (point. p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (+ (* x1 x2) (* y1 y2))))

(define (point- p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (point (- x1 x2) (- y1 y2))))

(define (point-norm p)
  (let ((len (point<> p)))
    (match p
      [(point x y)
       (point (/ x len) (/ y len))])))

(define (point= p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (and (= x1 x2)
         (= y1 y2))))

(define (point<> p)
  (match p
    [(point x y) (sqrt (+ (* x x) (* y y)))]))

(define (point-on-line-between p line-start line-end)
  (if (or (point= p line-start)
          (point= p line-end)) #t
                               (let* ((on^s (point- line-end line-start))
                                      (p^s (point- p line-start)))
                                 (if (point= (point-norm on^s) (point-norm p^s))
                                     (<= (point<> p^s) (point<> on^s))
                                     #f))))

(define (point-in-room? a-point a-room)
  (match-let ([(point x y) a-point]
              [(room rx ry rw rh) a-room])
    (and (>= x rx)
         (<= x (+ rx rw))
         (>= y ry)
         (<= y (+ ry rh)))))

(define room->points
  (match-lambda
    ([room x y w h]
     (list
      (point x y)
      (point (+ x w) y)
      (point x (+ y h))
      (point (+ x w) (+ y h))))))

(define (rooms-intersect? r1 r2)
  (any-by (room->points r1)
               (lambda (pt)
                 (point-in-room? pt r2))))

(simple-state-struct turtle 
  position direction features helicity)

(define (make-state-accessor fun)
  (lambda (state) (list (fun state) state)))

(define facings (vector 'east 'north 'west 'south))
(define up 1)
(define down -1)
(define (dir+ dir)
  (match dir
    ['east 'north]
    ['north 'west]
    ['west 'south]
    ['south 'east]))
(define (dir- dir)
  (match dir
    ['east 'south]
    ['south 'west]
    ['west 'north]
    ['north 'east]))

(define (turn dir)
  (lambda (state)
    (match-let ([(turtle pos face features helicity) state])
      (match (* dir helicity)
        [1 ((adjust-turtle-direction^ (dir+ face)) state)]
        [-1 ((adjust-turtle-direction^ (dir- face)) state)]))))

(define (turtle-nop state)
  (list 'no-op state))

(define (turn-n dir n)
  (if (= n 0) turtle-direction^
      (mlet* m-state
             ((face (turn dir)))
             (turn-n dir (- n 1)))))

(define (spin)
  (let ((n (random 4)))
    (mlet* m-state
           ((new-face (turn-n up n)))
           (m-return new-face))))

(define (stride min max)
  (let ((n (+ min (random (- max min)))))
    (mlet* m-state 
           ((new-position (move n)))
           (m-return new-position))))

(define (bounded-stride min max region)
  (let ((n (+ min (random (- max min)))))
    (mlet* m-state 
           ((new-position (move-within n region)))
           (m-return new-position))))

(define cardinal->point 
  (match-lambda 
    ['north (point  0  1)]
    ['south (point  0 -1)]
    ['east  (point  1  0)]
    ['west  (point -1  0)]))

(define (point+ p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (point (+ x1 x2)
           (+ y1 y2))))

(define print-turtle
  (match-lambda 
    [(turtle pos face features helicity)
     (eprintf "pos: ~a ~a~nface: ~a~nfeatures: ~a~nhelicity: ~a~n"
              (point-x pos)
              (point-y pos)
              face
              (features->string features)
              helicity)]))
              


(define (move n)
  (cond ((= n 0) turtle-position^)
        ((= n 1) (mlet* m-state
                        ((pos turtle-position^)
                         (facing turtle-direction^))
                        (adjust-turtle-position^ (point+ pos (cardinal->point facing)))))
        (else
         (mlet*
          m-state
          ((new-pos (move 1)))
          (move (- n 1))))))

(define (move-within n region)
  (cond ((= n 0) turtle-position^)
        ((= n 1) (mlet* m-state
                        ((pos turtle-position^)
                         (facing turtle-direction^))
                        (adjust-turtle-position^ (point+ pos (cardinal->point facing)))))
        (else
         (mlet*
          m-state
          ((pos turtle-position^)
           (facing turtle-direction^)
           (non-monadically:
            ((maybe-new-pos (point+ pos (cardinal->point facing)))
             (pos-ok? (point-in-room? maybe-new-pos region)))))
          (if pos-ok? 
              (mlet* m-state
                     ((_ (teleport maybe-new-pos)))
                     (move-within (- n 1) region))
              (m-return pos))))))
                     
                 
(define (turtle-features-dip a-turtle f)
  (struct-copy turtle a-turtle [features (f (turtle-features a-turtle))]))

(define (add-room r)
  (lambda (state)
    (list r (match r
              [(room x y w h) (turtle-features-dip state (>partial cons r))]))))

(define (just-rooms features)
  (filter room? features))

(define (room-in-region? a-room containing-room)
  (let ((pred (lambda (x)
                (point-in-room? x containing-room))))
  (match (room->points a-room) 
    [(list p1 p2 p3 p4)
     (and (pred p1)
          (pred p2)
          (pred p3)
          (pred p4))])))

(define (try-to-add-room r)
  (match-lambda 
    [(turtle pos face features helicity)
     (cond ((empty? features)
            (list #t (turtle pos face (list r) helicity)))
           (else
            (if (none-by (just-rooms features) (partial< rooms-intersect? r))
                (list #t (turtle pos face (cons r features) helicity))
                (list #f (turtle pos face features helicity)))))]))

(define (try-to-add-room-in-region r region)
  (match-lambda 
    [(turtle pos face features helicity)
     (cond ((empty? features)
            (list #t (turtle pos face (list r) helicity)))
           ((not (room-in-region? r region))
            (list #f (turtle pos face (list r) helicity)))
           (else
            (if (none-by (just-rooms features) (partial< rooms-intersect? r))
                (list #t (turtle pos face (cons r features) helicity))
                (list #f (turtle pos face features helicity)))))]))

(define (add-hall-segment h)
  (lambda (state)
    (match-let ([(hall-segment p1 p2) h]
                [(turtle pos face features hel) state])
      (list #t (turtle pos face 
                       (cons h features) hel)))))
                       
(define (move-hallway n)
  (mlet* m-state
         ((start turtle-position^)
          (end (move n))
          (h (add-hall-segment (hall-segment start end))))
         (m-return h)))

(define (room-here w h)
  (mlet* m-state 
         ((non-monadically: 
           ((hw (ceiling (/ w 2)))
            (hh (ceiling (/ h 2)))))
          (pos turtle-position^)
          (non-monadically: 
           ((bottom-left (point- pos (point hw hh)))))
          (success? (try-to-add-room 
                     (room (point-x bottom-left)
                           (point-y bottom-left)
                           w 
                           h))))
         (m-return success?)))

(define (room-here-in-region w h region)
  (mlet* m-state 
         ((non-monadically: 
           ((hw (ceiling (/ w 2)))
            (hh (ceiling (/ h 2)))))
          (pos turtle-position^)
          (non-monadically: 
           ((bottom-left (point- pos (point hw hh)))))
          (success? (try-to-add-room-in-region 
                     (room (point-x bottom-left)
                           (point-y bottom-left)
                           w 
                           h) region)))
         (m-return success?)))
            

(define teleport
  adjust-turtle-position^)


(define (point-in-hallway? a-point a-hallway)
  (match-let ([(hall-segment hall-start hall-stop) a-hallway]
              [(point x y) a-point])
    (point-on-line-between a-point hall-start hall-stop)))

(define (point-in-feature? point feature)
  (cond ((hall-segment? feature)
         (point-in-hallway? point feature))
        ((room? feature)
         (point-in-room? point feature))))

(define (point-in-features? point features)
  (cond ((empty? features) #f)
        (else (any-by features
                (lambda (feature)
                  (point-in-feature? point feature))))))

(define (point-in-wall? a-point)
  (mlet* m-state 
         ((features turtle-features^))
         (m-return (not (point-in-features? a-point features)))))

(define (pick-cadr lst f)
  (f (cadr lst)))

(define starting-turtle
  (turtle (point 32 16)
          'north
          '()
          1))

(define (move-to-wall-limited max-steps)
   (if (= max-steps 0) turtle-position^
       (mlet* m-state
              ((pos turtle-position^)
               (already-in-wall? (point-in-wall? pos))
               (facing turtle-direction^)
               (non-monadically: ((new-pos 
                                   (point+ pos (cardinal->point facing)))))
               (wall? (point-in-wall? new-pos)))
              (displayln
               (format "old ~a, new ~a, wall? ~a, already-in-wall? ~a"
                       (feature->string pos)
                       (feature->string new-pos) 
                       wall?
                       already-in-wall?))
              (if (or wall? already-in-wall?) 
                  (begin (print pos) (m-return pos))
                  (mlet* m-state
                         ((_ (teleport new-pos)))
                         (move-to-wall-limited (- max-steps 1)))))))

(define feature->string
  (match-lambda
    [(point x y) (format "point: ~a ~a" x y)]
    [(hall-segment p1 p2)
     (format "hall-segment: <~a> to <~a>" (feature->string p1) (feature->string p2))]
    [(room x y w h) (format "room: ~a ~a ~a ~a" x y w h)]))

(define (features->string features)
  (foldl (lambda (feature out)
           (string-append out (format "~n")
            (feature->string feature)))
         ""
         features))
     

(define move-to-wall 
  (mlet* m-state 
         ((pos turtle-position^)
          (face turtle-direction^)
          (non-monadically:
           ((one-forward
             (point+ pos (cardinal->point face)))))
          (bool (point-in-wall? one-forward)))
         (if bool (m-return pos)
             (mlet* m-state
                    ((np (teleport one-forward)))
                    (print "stepping")
                    move-to-wall))))


                     


        
(define test
  (mlet* m-state
         ((p (teleport (point 0 0)))
          (r (room-here 12 16))
          (p move-to-wall))
         (m-return p)))

(define (random-room-dim)
  (+ 4 (* 2 (random 5))))

(define (get-random-room-dim)
  (let ((d (random-room-dim)))
    (lambda (state)
      (list d state))))

(define step
  (mlet* m-state
         ((w (get-random-room-dim))
          (h (get-random-room-dim))
          (r (room-here-in-region w h (room 0 0 64 32)))
          (face (spin))
          (p (move-to-wall-limited 100))
          (p2 (bounded-stride 5 25 (room 0 0 64 32)))
          (h (add-hall-segment (hall-segment p p2))))
         (m-return p2)))

(define (n-times state-fun n)
  (with-monad m-state
              (if (= n 0) (m-return n)
                  (mlet* m-state
                         ((_ state-fun))
                         (n-times state-fun (- n 1))))))

;(define new-turtle (cadr (step starting-turtle)))
;(define stepped-turtle (cadr ((n-times step 4) starting-turtle)))

(provide (all-defined-out))