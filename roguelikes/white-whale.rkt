#lang racket

(require racket/gui
         racket/planar-geometry
         racket/class)

(define game-width (* 16 32))
(define game-height (* 16 32))

(define (what-is-point-in? pt lst)
  (let loop ([lst lst]
             [acc 'nothing])
    (match lst
      [(list) acc]
      [(cons head tail)
       (if (point-in-shape? pt head) 
           (match (shape-type head)
             ['rectangle (loop tail 'rectangle)]
             ['line-segment (loop tail
                                  (if (eq? acc 'rectangle) 'rectangle
                                      'line-segment))])
           (loop tail acc))])))
  

(define floor 
  (class object%
    (init geometry)
    (init width)
    (init height)
    (define grid 
      (let ((g (make-hash)))
        (let i-loop ((i 0))
          (let j-loop ((j 0))
            (if 

(define rl-world% 
  (class object%
    (define things '())
    (define current-floot #f)
    

(define game-frame 
  (new frame%
       [width game-width]
       [height game-height]
       [label "Whale"]))

(define game-canvas 
  (new canvas%
       [width game-width]
       [height game-height]
       [paint-callback 
        (lambda (self dc)
          (send worldview paint-onto dc))]))



