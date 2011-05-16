#lang racket
(require utilities/geometry
         utilities/fancy-destructuring
         utilities/lists
         functional/point-free
         utilities/one-of)

(define (room! x y w h)
  (list 'room x y w h)) 

(define (uniform min width)
  (+ min (random width)))

(define (random-room within max-width max-height)
  (dlet1
   (: x-min y-min x-w y-w) within
   (let* ((x (uniform x-min x-w))
          (y (uniform y-min y-w))
          (w (uniform 3 max-width))
          (h (uniform 3 max-height))
          (left (+ x w))
          (top (+ y h)))
     (if (or (> left (+ x-min x-w))
             (> top (+ y-min y-w)))
         (random-room within max-width max-height)
         (room! x y w h)))))

(defn (room->box (: _ x y w h))
  (list (pt! x y)
        (pt! (+ x w) y)
        (pt! x (+ y h))
        (pt! (+ x w) (+ y w))))

(define (rooms-overlap? room-1 room-2)
  (apply box-intersect? (map room->box (list room-1 room-2))))

(define (random-room-avoiding 
         within
         max-width 
         max-height 
         room-list)
  (if (empty? room-list)
      (random-room within max-width max-height)
      (elet [(: x-min y-min x-w y-h) within]
            (let loop ((candidate (random-room 
                                   within max-width max-height)))
              (if (not (any-by room-list 
                               (partial< rooms-overlap? candidate)))
                  candidate
                  (loop (random-room within max-width max-height)))))))

(defn (rooms-share-x-silouette? 
       (: _ x1 y1 w1 h1)
       (: _ x2 y2 w2 h2))
  (rooms-overlap?
   (room! x1 y1 w1 h1)
   (room! x2 y1 w2 h1)))

(defn (rooms-share-y-silouette? 
       (: _ x1 y1 w1 h1)
       (: _ x2 y2 w2 h2))
  (rooms-overlap?
   (room! x1 y1 w1 h1)
   (room! x1 y2 w1 h2)))

(defn (room-center (: _ x y w h))
  (pt! (+ x (/ w 2))
       (+ y (/ h 2))))

(defn (room-center-distance r1 r2)
  (pt|| (room-center r1)
        (room-center r2)))

(define (n-random-rooms within max-width max-height n)
  (let loop ((rooms '())
             (i 0))
    (if (= i n) rooms
        (loop (cons (random-room-avoiding within max-width max-height rooms) rooms) (+ i 1)))))



(provide n-random-rooms random-room-avoiding random-room)