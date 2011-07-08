#lang racket

(require utilities/lists
         racket/match
         racket/dict
         racket/gui
         utilities/simple-infix)

(struct stone (x y index color))
(struct board (size stones))

(define black 'black)
(define white 'white)

(define (strictly-between n min max)
  (and (> n min)
       (< n max)))

(define (position-on-board? a-board x y)
  (and ($ x strictly-between -1 (board-size a-board))
       ($ y strictly-between -1 (board-size a-board))))

(define (add-stone a-board color x y)
  (if (and
       (x y-on-board? a-board x y)
       (x y-empty? a-board x y))
      (copy-struct board a-board
                   [stones
                    (add-to-front
                     (stone x y (length (board-stones a-board))
                            color)
                     (board-stones a-board))])
      #f))



