#lang racket
(require racket/gui
         racket/class
         racket/dict
         utilities/sprite-loading)

(define character-sheet "./oryx-lofi/x4-lofi_char.png")
(define character-sheet-type 'png/alpha)
(define x4-characters (make-hash))

(dict-set! x4-characters 'normal-0-0 
           (load-image character-sheet character-sheet-type 0 0 1 1 32 32))
(dict-set! x4-characters 'human-ranger (dict-ref x4-characters 'normal-0-0))

(dict-set! x4-characters 'normal-0-1 
           (load-image character-sheet character-sheet-type 0 1 1 1 32 32))
(dict-set! x4-characters 'human-soldier (dict-ref x4-characters 'normal-0-1))

(dict-set! x4-characters 'normal-0-2 
           (load-image character-sheet character-sheet-type 0 2 1 1 32 32))
(dict-set! x4-characters 'human-mage (dict-ref x4-characters 'normal-0-2))

(dict-set! x4-characters 'normal-0-3 
           (load-image character-sheet character-sheet-type 0 3 1 1 32 32))
(dict-set! x4-characters 'human-rogue (dict-ref x4-characters 'normal-0-3))

(dict-set! x4-characters 'normal-0-4 
           (load-image character-sheet character-sheet-type 0 4 1 1 32 32))
(dict-set! x4-characters 'human-red-mage (dict-ref x4-characters 'normal-0-4))

(dict-set! x4-characters 'normal-0-5 
           (load-image character-sheet character-sheet-type 0 5 1 1 32 32))
(dict-set! x4-characters 'human-paladin (dict-ref x4-characters 'normal-0-5))

(dict-set! x4-characters 'normal-0-6 
           (load-image character-sheet character-sheet-type 0 6 1 1 32 32))
(dict-set! x4-characters 'normal-0-7 
           (load-image character-sheet character-sheet-type 0 7 1 1 32 32))
(dict-set! x4-characters 'normal-0-8 
           (load-image character-sheet character-sheet-type 0 8 1 1 32 32))
(dict-set! x4-characters 'normal-0-9 
           (load-image character-sheet character-sheet-type 0 9 1 1 32 32))
(dict-set! x4-characters 'normal-0-10 
           (load-image character-sheet character-sheet-type 0 10 1 1 32 32))
(dict-set! x4-characters 'normal-0-11 
           (load-image character-sheet character-sheet-type 0 11 1 1 32 32))
(dict-set! x4-characters 'normal-0-12 
           (load-image character-sheet character-sheet-type 0 12 1 1 32 32))
(dict-set! x4-characters 'normal-0-13 
           (load-image character-sheet character-sheet-type 0 13 1 1 32 32))
(dict-set! x4-characters 'normal-0-14 
           (load-image character-sheet character-sheet-type 0 14 1 1 32 32))

(dict-set! x4-characters 'normal-1-0 
           (load-image character-sheet character-sheet-type 1 0 1 1 32 32))
(dict-set! x4-characters 'normal-1-1 
           (load-image character-sheet character-sheet-type 1 1 1 1 32 32))
(dict-set! x4-characters 'normal-1-2 
           (load-image character-sheet character-sheet-type 1 2 1 1 32 32))
(dict-set! x4-characters 'normal-1-3 
           (load-image character-sheet character-sheet-type 1 3 1 1 32 32))
(dict-set! x4-characters 'normal-1-4 
           (load-image character-sheet character-sheet-type 1 4 1 1 32 32))
(dict-set! x4-characters 'normal-1-5 
           (load-image character-sheet character-sheet-type 1 5 1 1 32 32))
(dict-set! x4-characters 'normal-1-6 
           (load-image character-sheet character-sheet-type 1 6 1 1 32 32))
(dict-set! x4-characters 'normal-1-7 
           (load-image character-sheet character-sheet-type 1 7 1 1 32 32))
(dict-set! x4-characters 'normal-1-8 
           (load-image character-sheet character-sheet-type 1 8 1 1 32 32))
(dict-set! x4-characters 'normal-1-9 
           (load-image character-sheet character-sheet-type 1 9 1 1 32 32))
(dict-set! x4-characters 'normal-1-10 
           (load-image character-sheet character-sheet-type 1 10 1 1 32 32))
(dict-set! x4-characters 'normal-1-11 
           (load-image character-sheet character-sheet-type 1 11 1 1 32 32))
(dict-set! x4-characters 'normal-1-12 
           (load-image character-sheet character-sheet-type 1 12 1 1 32 32))
(dict-set! x4-characters 'normal-1-13 
           (load-image character-sheet character-sheet-type 1 13 1 1 32 32))
(dict-set! x4-characters 'normal-1-14 
           (load-image character-sheet character-sheet-type 1 14 1 1 32 32))

(dict-set! x4-characters 'normal-2-0 
           (load-image character-sheet character-sheet-type 2 0 1 1 32 32))
(dict-set! x4-characters 'normal-2-1 
           (load-image character-sheet character-sheet-type 2 1 1 1 32 32))
(dict-set! x4-characters 'normal-2-2 
           (load-image character-sheet character-sheet-type 2 2 1 1 32 32))
(dict-set! x4-characters 'normal-2-3 
           (load-image character-sheet character-sheet-type 2 3 1 1 32 32))
(dict-set! x4-characters 'normal-2-4 
           (load-image character-sheet character-sheet-type 2 4 1 1 32 32))
(dict-set! x4-characters 'normal-2-5 
           (load-image character-sheet character-sheet-type 2 5 1 1 32 32))
(dict-set! x4-characters 'normal-2-6 
           (load-image character-sheet character-sheet-type 2 6 1 1 32 32))
(dict-set! x4-characters 'normal-2-7 
           (load-image character-sheet character-sheet-type 2 7 1 1 32 32))
(dict-set! x4-characters 'normal-2-8 
           (load-image character-sheet character-sheet-type 2 8 1 1 32 32))
(dict-set! x4-characters 'normal-2-9 
           (load-image character-sheet character-sheet-type 2 9 1 1 32 32))
(dict-set! x4-characters 'normal-2-10 
           (load-image character-sheet character-sheet-type 2 10 1 1 32 32))
(dict-set! x4-characters 'normal-2-11 
           (load-image character-sheet character-sheet-type 2 11 1 1 32 32))
(dict-set! x4-characters 'normal-2-12 
           (load-image character-sheet character-sheet-type 2 12 1 1 32 32))
(dict-set! x4-characters 'normal-2-13 
           (load-image character-sheet character-sheet-type 2 13 1 1 32 32))
(dict-set! x4-characters 'normal-2-14 
           (load-image character-sheet character-sheet-type 2 14 1 1 32 32))

(dict-set! x4-characters 'normal-3-0 
           (load-image character-sheet character-sheet-type 3 0 1 1 32 32))
(dict-set! x4-characters 'normal-3-1 
           (load-image character-sheet character-sheet-type 3 1 1 1 32 32))
(dict-set! x4-characters 'normal-3-2 
           (load-image character-sheet character-sheet-type 3 2 1 1 32 32))
(dict-set! x4-characters 'normal-3-3 
           (load-image character-sheet character-sheet-type 3 3 1 1 32 32))
(dict-set! x4-characters 'normal-3-4 
           (load-image character-sheet character-sheet-type 3 4 1 1 32 32))
(dict-set! x4-characters 'normal-3-5 
           (load-image character-sheet character-sheet-type 3 5 1 1 32 32))
(dict-set! x4-characters 'normal-3-6 
           (load-image character-sheet character-sheet-type 3 6 1 1 32 32))
(dict-set! x4-characters 'normal-3-7 
           (load-image character-sheet character-sheet-type 3 7 1 1 32 32))
(dict-set! x4-characters 'normal-3-8 
           (load-image character-sheet character-sheet-type 3 8 1 1 32 32))
(dict-set! x4-characters 'normal-3-9 
           (load-image character-sheet character-sheet-type 3 9 1 1 32 32))
(dict-set! x4-characters 'normal-3-10 
           (load-image character-sheet character-sheet-type 3 10 1 1 32 32))
(dict-set! x4-characters 'normal-3-11 
           (load-image character-sheet character-sheet-type 3 11 1 1 32 32))
(dict-set! x4-characters 'normal-3-12 
           (load-image character-sheet character-sheet-type 3 12 1 1 32 32))
(dict-set! x4-characters 'normal-3-13 
           (load-image character-sheet character-sheet-type 3 13 1 1 32 32))
(dict-set! x4-characters 'normal-3-14 
           (load-image character-sheet character-sheet-type 3 14 1 1 32 32))

(dict-set! x4-characters 'normal-4-0 
           (load-image character-sheet character-sheet-type 4 0 1 1 32 32))
(dict-set! x4-characters 'normal-4-1 
           (load-image character-sheet character-sheet-type 4 1 1 1 32 32))
(dict-set! x4-characters 'normal-4-2 
           (load-image character-sheet character-sheet-type 4 2 1 1 32 32))
(dict-set! x4-characters 'normal-4-3 
           (load-image character-sheet character-sheet-type 4 3 1 1 32 32))
(dict-set! x4-characters 'normal-4-4 
           (load-image character-sheet character-sheet-type 4 4 1 1 32 32))
(dict-set! x4-characters 'normal-4-5 
           (load-image character-sheet character-sheet-type 4 5 1 1 32 32))
(dict-set! x4-characters 'normal-4-6 
           (load-image character-sheet character-sheet-type 4 6 1 1 32 32))
(dict-set! x4-characters 'normal-4-7 
           (load-image character-sheet character-sheet-type 4 7 1 1 32 32))
(dict-set! x4-characters 'normal-4-8 
           (load-image character-sheet character-sheet-type 4 8 1 1 32 32))
(dict-set! x4-characters 'normal-4-9 
           (load-image character-sheet character-sheet-type 4 9 1 1 32 32))
(dict-set! x4-characters 'normal-4-10 
           (load-image character-sheet character-sheet-type 4 10 1 1 32 32))
(dict-set! x4-characters 'normal-4-11 
           (load-image character-sheet character-sheet-type 4 11 1 1 32 32))
(dict-set! x4-characters 'normal-4-12 
           (load-image character-sheet character-sheet-type 4 12 1 1 32 32))
(dict-set! x4-characters 'normal-4-13 
           (load-image character-sheet character-sheet-type 4 13 1 1 32 32))
(dict-set! x4-characters 'normal-4-14 
           (load-image character-sheet character-sheet-type 4 14 1 1 32 32))

(dict-set! x4-characters 'normal-5-0 
           (load-image character-sheet character-sheet-type 5 0 1 1 32 32))
(dict-set! x4-characters 'normal-5-1 
           (load-image character-sheet character-sheet-type 5 1 1 1 32 32))
(dict-set! x4-characters 'normal-5-2 
           (load-image character-sheet character-sheet-type 5 2 1 1 32 32))
(dict-set! x4-characters 'normal-5-3 
           (load-image character-sheet character-sheet-type 5 3 1 1 32 32))
(dict-set! x4-characters 'normal-5-4 
           (load-image character-sheet character-sheet-type 5 4 1 1 32 32))
(dict-set! x4-characters 'normal-5-5 
           (load-image character-sheet character-sheet-type 5 5 1 1 32 32))
(dict-set! x4-characters 'normal-5-6 
           (load-image character-sheet character-sheet-type 5 6 1 1 32 32))
(dict-set! x4-characters 'normal-5-7 
           (load-image character-sheet character-sheet-type 5 7 1 1 32 32))
(dict-set! x4-characters 'normal-5-8 
           (load-image character-sheet character-sheet-type 5 8 1 1 32 32))
(dict-set! x4-characters 'normal-5-9 
           (load-image character-sheet character-sheet-type 5 9 1 1 32 32))
(dict-set! x4-characters 'normal-5-10 
           (load-image character-sheet character-sheet-type 5 10 1 1 32 32))
(dict-set! x4-characters 'normal-5-11 
           (load-image character-sheet character-sheet-type 5 11 1 1 32 32))
(dict-set! x4-characters 'normal-5-12 
           (load-image character-sheet character-sheet-type 5 12 1 1 32 32))
(dict-set! x4-characters 'normal-5-13 
           (load-image character-sheet character-sheet-type 5 13 1 1 32 32))
(dict-set! x4-characters 'normal-5-14 
           (load-image character-sheet character-sheet-type 5 14 1 1 32 32))

(dict-set! x4-characters 'normal-6-0 
           (load-image character-sheet character-sheet-type 6 0 1 1 32 32))
(dict-set! x4-characters 'normal-6-1 
           (load-image character-sheet character-sheet-type 6 1 1 1 32 32))
(dict-set! x4-characters 'normal-6-2 
           (load-image character-sheet character-sheet-type 6 2 1 1 32 32))
(dict-set! x4-characters 'normal-6-3 
           (load-image character-sheet character-sheet-type 6 3 1 1 32 32))
(dict-set! x4-characters 'normal-6-4 
           (load-image character-sheet character-sheet-type 6 4 1 1 32 32))
(dict-set! x4-characters 'normal-6-5 
           (load-image character-sheet character-sheet-type 6 5 1 1 32 32))
(dict-set! x4-characters 'normal-6-6 
           (load-image character-sheet character-sheet-type 6 6 1 1 32 32))
(dict-set! x4-characters 'normal-6-7 
           (load-image character-sheet character-sheet-type 6 7 1 1 32 32))
(dict-set! x4-characters 'normal-6-8 
           (load-image character-sheet character-sheet-type 6 8 1 1 32 32))
(dict-set! x4-characters 'normal-6-9 
           (load-image character-sheet character-sheet-type 6 9 1 1 32 32))
(dict-set! x4-characters 'normal-6-10 
           (load-image character-sheet character-sheet-type 6 10 1 1 32 32))
(dict-set! x4-characters 'normal-6-11 
           (load-image character-sheet character-sheet-type 6 11 1 1 32 32))
(dict-set! x4-characters 'normal-6-12 
           (load-image character-sheet character-sheet-type 6 12 1 1 32 32))
(dict-set! x4-characters 'normal-6-13 
           (load-image character-sheet character-sheet-type 6 13 1 1 32 32))
(dict-set! x4-characters 'normal-6-14 
           (load-image character-sheet character-sheet-type 6 14 1 1 32 32))

(dict-set! x4-characters 'normal-7-0 
           (load-image character-sheet character-sheet-type 7 0 1 1 32 32))
(dict-set! x4-characters 'normal-7-1 
           (load-image character-sheet character-sheet-type 7 1 1 1 32 32))
(dict-set! x4-characters 'normal-7-2 
           (load-image character-sheet character-sheet-type 7 2 1 1 32 32))
(dict-set! x4-characters 'normal-7-3 
           (load-image character-sheet character-sheet-type 7 3 1 1 32 32))
(dict-set! x4-characters 'normal-7-4 
           (load-image character-sheet character-sheet-type 7 4 1 1 32 32))
(dict-set! x4-characters 'normal-7-5 
           (load-image character-sheet character-sheet-type 7 5 1 1 32 32))
(dict-set! x4-characters 'normal-7-6 
           (load-image character-sheet character-sheet-type 7 6 1 1 32 32))
(dict-set! x4-characters 'normal-7-7 
           (load-image character-sheet character-sheet-type 7 7 1 1 32 32))
(dict-set! x4-characters 'normal-7-8 
           (load-image character-sheet character-sheet-type 7 8 1 1 32 32))
(dict-set! x4-characters 'normal-7-9 
           (load-image character-sheet character-sheet-type 7 9 1 1 32 32))
(dict-set! x4-characters 'normal-7-10 
           (load-image character-sheet character-sheet-type 7 10 1 1 32 32))
(dict-set! x4-characters 'normal-7-11 
           (load-image character-sheet character-sheet-type 7 11 1 1 32 32))
(dict-set! x4-characters 'normal-7-12 
           (load-image character-sheet character-sheet-type 7 12 1 1 32 32))
(dict-set! x4-characters 'normal-7-13 
           (load-image character-sheet character-sheet-type 7 13 1 1 32 32))
(dict-set! x4-characters 'normal-7-14 
           (load-image character-sheet character-sheet-type 7 14 1 1 32 32))

(dict-set! x4-characters 'normal-8-0 
           (load-image character-sheet character-sheet-type 8 0 1 1 32 32))
(dict-set! x4-characters 'normal-8-1 
           (load-image character-sheet character-sheet-type 8 1 1 1 32 32))
(dict-set! x4-characters 'normal-8-2 
           (load-image character-sheet character-sheet-type 8 2 1 1 32 32))
(dict-set! x4-characters 'normal-8-3 
           (load-image character-sheet character-sheet-type 8 3 1 1 32 32))
(dict-set! x4-characters 'normal-8-4 
           (load-image character-sheet character-sheet-type 8 4 1 1 32 32))
(dict-set! x4-characters 'normal-8-5 
           (load-image character-sheet character-sheet-type 8 5 1 1 32 32))
(dict-set! x4-characters 'normal-8-6 
           (load-image character-sheet character-sheet-type 8 6 1 1 32 32))
(dict-set! x4-characters 'normal-8-7 
           (load-image character-sheet character-sheet-type 8 7 1 1 32 32))
(dict-set! x4-characters 'normal-8-8 
           (load-image character-sheet character-sheet-type 8 8 1 1 32 32))
(dict-set! x4-characters 'normal-8-9 
           (load-image character-sheet character-sheet-type 8 9 1 1 32 32))
(dict-set! x4-characters 'normal-8-10 
           (load-image character-sheet character-sheet-type 8 10 1 1 32 32))
(dict-set! x4-characters 'normal-8-11 
           (load-image character-sheet character-sheet-type 8 11 1 1 32 32))
(dict-set! x4-characters 'normal-8-12 
           (load-image character-sheet character-sheet-type 8 12 1 1 32 32))
(dict-set! x4-characters 'normal-8-13 
           (load-image character-sheet character-sheet-type 8 13 1 1 32 32))
(dict-set! x4-characters 'normal-8-14 
           (load-image character-sheet character-sheet-type 8 14 1 1 32 32))

(dict-set! x4-characters 'normal-9-0 
           (load-image character-sheet character-sheet-type 9 0 1 1 32 32))
(dict-set! x4-characters 'normal-9-1 
           (load-image character-sheet character-sheet-type 9 1 1 1 32 32))
(dict-set! x4-characters 'normal-9-2 
           (load-image character-sheet character-sheet-type 9 2 1 1 32 32))
(dict-set! x4-characters 'normal-9-3 
           (load-image character-sheet character-sheet-type 9 3 1 1 32 32))
(dict-set! x4-characters 'normal-9-4 
           (load-image character-sheet character-sheet-type 9 4 1 1 32 32))
(dict-set! x4-characters 'normal-9-5 
           (load-image character-sheet character-sheet-type 9 5 1 1 32 32))
(dict-set! x4-characters 'normal-9-6 
           (load-image character-sheet character-sheet-type 9 6 1 1 32 32))
(dict-set! x4-characters 'normal-9-7 
           (load-image character-sheet character-sheet-type 9 7 1 1 32 32))
(dict-set! x4-characters 'normal-9-8 
           (load-image character-sheet character-sheet-type 9 8 1 1 32 32))
(dict-set! x4-characters 'normal-9-9 
           (load-image character-sheet character-sheet-type 9 9 1 1 32 32))
(dict-set! x4-characters 'normal-9-10 
           (load-image character-sheet character-sheet-type 9 10 1 1 32 32))
(dict-set! x4-characters 'normal-9-11 
           (load-image character-sheet character-sheet-type 9 11 1 1 32 32))
(dict-set! x4-characters 'normal-9-12 
           (load-image character-sheet character-sheet-type 9 12 1 1 32 32))
(dict-set! x4-characters 'normal-9-13 
           (load-image character-sheet character-sheet-type 9 13 1 1 32 32))
(dict-set! x4-characters 'normal-9-14 
           (load-image character-sheet character-sheet-type 9 14 1 1 32 32))

(dict-set! x4-characters 'normal-10-0 
           (load-image character-sheet character-sheet-type 10 0 1 1 32 32))
(dict-set! x4-characters 'normal-10-1 
           (load-image character-sheet character-sheet-type 10 1 1 1 32 32))
(dict-set! x4-characters 'normal-10-2 
           (load-image character-sheet character-sheet-type 10 2 1 1 32 32))
(dict-set! x4-characters 'normal-10-3 
           (load-image character-sheet character-sheet-type 10 3 1 1 32 32))
(dict-set! x4-characters 'normal-10-4 
           (load-image character-sheet character-sheet-type 10 4 1 1 32 32))
(dict-set! x4-characters 'normal-10-5 
           (load-image character-sheet character-sheet-type 10 5 1 1 32 32))
(dict-set! x4-characters 'normal-10-6 
           (load-image character-sheet character-sheet-type 10 6 1 1 32 32))
(dict-set! x4-characters 'normal-10-7 
           (load-image character-sheet character-sheet-type 10 7 1 1 32 32))
(dict-set! x4-characters 'normal-10-8 
           (load-image character-sheet character-sheet-type 10 8 1 1 32 32))
(dict-set! x4-characters 'normal-10-9 
           (load-image character-sheet character-sheet-type 10 9 1 1 32 32))
(dict-set! x4-characters 'normal-10-10 
           (load-image character-sheet character-sheet-type 10 10 1 1 32 32))
(dict-set! x4-characters 'normal-10-11 
           (load-image character-sheet character-sheet-type 10 11 1 1 32 32))
(dict-set! x4-characters 'normal-10-12 
           (load-image character-sheet character-sheet-type 10 12 1 1 32 32))
(dict-set! x4-characters 'normal-10-13 
           (load-image character-sheet character-sheet-type 10 13 1 1 32 32))
(dict-set! x4-characters 'normal-10-14 
           (load-image character-sheet character-sheet-type 10 14 1 1 32 32))

(dict-set! x4-characters 'normal-11-0 
           (load-image character-sheet character-sheet-type 11 0 1 1 32 32))
(dict-set! x4-characters 'normal-11-1 
           (load-image character-sheet character-sheet-type 11 1 1 1 32 32))
(dict-set! x4-characters 'normal-11-2 
           (load-image character-sheet character-sheet-type 11 2 1 1 32 32))
(dict-set! x4-characters 'normal-11-3 
           (load-image character-sheet character-sheet-type 11 3 1 1 32 32))
(dict-set! x4-characters 'normal-11-4 
           (load-image character-sheet character-sheet-type 11 4 1 1 32 32))
(dict-set! x4-characters 'normal-11-5 
           (load-image character-sheet character-sheet-type 11 5 1 1 32 32))
(dict-set! x4-characters 'normal-11-6 
           (load-image character-sheet character-sheet-type 11 6 1 1 32 32))
(dict-set! x4-characters 'normal-11-7 
           (load-image character-sheet character-sheet-type 11 7 1 1 32 32))
(dict-set! x4-characters 'normal-11-8 
           (load-image character-sheet character-sheet-type 11 8 1 1 32 32))
(dict-set! x4-characters 'normal-11-9 
           (load-image character-sheet character-sheet-type 11 9 1 1 32 32))
(dict-set! x4-characters 'normal-11-10 
           (load-image character-sheet character-sheet-type 11 10 1 1 32 32))
(dict-set! x4-characters 'normal-11-11 
           (load-image character-sheet character-sheet-type 11 11 1 1 32 32))
(dict-set! x4-characters 'normal-11-12 
           (load-image character-sheet character-sheet-type 11 12 1 1 32 32))
(dict-set! x4-characters 'normal-11-13 
           (load-image character-sheet character-sheet-type 11 13 1 1 32 32))
(dict-set! x4-characters 'normal-11-14 
           (load-image character-sheet character-sheet-type 11 14 1 1 32 32))

(dict-set! x4-characters 'normal-12-0 
           (load-image character-sheet character-sheet-type 12 0 1 1 32 32))
(dict-set! x4-characters 'normal-12-1 
           (load-image character-sheet character-sheet-type 12 1 1 1 32 32))
(dict-set! x4-characters 'normal-12-2 
           (load-image character-sheet character-sheet-type 12 2 1 1 32 32))
(dict-set! x4-characters 'normal-12-3 
           (load-image character-sheet character-sheet-type 12 3 1 1 32 32))
(dict-set! x4-characters 'normal-12-4 
           (load-image character-sheet character-sheet-type 12 4 1 1 32 32))
(dict-set! x4-characters 'normal-12-5 
           (load-image character-sheet character-sheet-type 12 5 1 1 32 32))
(dict-set! x4-characters 'normal-12-6 
           (load-image character-sheet character-sheet-type 12 6 1 1 32 32))
(dict-set! x4-characters 'normal-12-7 
           (load-image character-sheet character-sheet-type 12 7 1 1 32 32))
(dict-set! x4-characters 'normal-12-8 
           (load-image character-sheet character-sheet-type 12 8 1 1 32 32))
(dict-set! x4-characters 'normal-12-9 
           (load-image character-sheet character-sheet-type 12 9 1 1 32 32))
(dict-set! x4-characters 'normal-12-10 
           (load-image character-sheet character-sheet-type 12 10 1 1 32 32))
(dict-set! x4-characters 'normal-12-11 
           (load-image character-sheet character-sheet-type 12 11 1 1 32 32))
(dict-set! x4-characters 'normal-12-12 
           (load-image character-sheet character-sheet-type 12 12 1 1 32 32))
(dict-set! x4-characters 'normal-12-13 
           (load-image character-sheet character-sheet-type 12 13 1 1 32 32))
(dict-set! x4-characters 'normal-12-14 
           (load-image character-sheet character-sheet-type 12 14 1 1 32 32))

(dict-set! x4-characters 'normal-13-0 
           (load-image character-sheet character-sheet-type 13 0 1 1 32 32))
(dict-set! x4-characters 'normal-13-1 
           (load-image character-sheet character-sheet-type 13 1 1 1 32 32))
(dict-set! x4-characters 'normal-13-2 
           (load-image character-sheet character-sheet-type 13 2 1 1 32 32))
(dict-set! x4-characters 'normal-13-3 
           (load-image character-sheet character-sheet-type 13 3 1 1 32 32))
(dict-set! x4-characters 'normal-13-4 
           (load-image character-sheet character-sheet-type 13 4 1 1 32 32))
(dict-set! x4-characters 'normal-13-5 
           (load-image character-sheet character-sheet-type 13 5 1 1 32 32))
(dict-set! x4-characters 'normal-13-6 
           (load-image character-sheet character-sheet-type 13 6 1 1 32 32))
(dict-set! x4-characters 'normal-13-7 
           (load-image character-sheet character-sheet-type 13 7 1 1 32 32))
(dict-set! x4-characters 'normal-13-8 
           (load-image character-sheet character-sheet-type 13 8 1 1 32 32))
(dict-set! x4-characters 'normal-13-9 
           (load-image character-sheet character-sheet-type 13 9 1 1 32 32))
(dict-set! x4-characters 'normal-13-10 
           (load-image character-sheet character-sheet-type 13 10 1 1 32 32))
(dict-set! x4-characters 'normal-13-11 
           (load-image character-sheet character-sheet-type 13 11 1 1 32 32))
(dict-set! x4-characters 'normal-13-12 
           (load-image character-sheet character-sheet-type 13 12 1 1 32 32))
(dict-set! x4-characters 'normal-13-13 
           (load-image character-sheet character-sheet-type 13 13 1 1 32 32))
(dict-set! x4-characters 'normal-13-14 
           (load-image character-sheet character-sheet-type 13 14 1 1 32 32))

(dict-set! x4-characters 'normal-14-0 
           (load-image character-sheet character-sheet-type 14 0 1 1 32 32))
(dict-set! x4-characters 'normal-14-1 
           (load-image character-sheet character-sheet-type 14 1 1 1 32 32))
(dict-set! x4-characters 'normal-14-2 
           (load-image character-sheet character-sheet-type 14 2 1 1 32 32))
(dict-set! x4-characters 'normal-14-3 
           (load-image character-sheet character-sheet-type 14 3 1 1 32 32))
(dict-set! x4-characters 'normal-14-4 
           (load-image character-sheet character-sheet-type 14 4 1 1 32 32))
(dict-set! x4-characters 'normal-14-5 
           (load-image character-sheet character-sheet-type 14 5 1 1 32 32))
(dict-set! x4-characters 'normal-14-6 
           (load-image character-sheet character-sheet-type 14 6 1 1 32 32))
(dict-set! x4-characters 'normal-14-7 
           (load-image character-sheet character-sheet-type 14 7 1 1 32 32))
(dict-set! x4-characters 'normal-14-8 
           (load-image character-sheet character-sheet-type 14 8 1 1 32 32))
(dict-set! x4-characters 'normal-14-9 
           (load-image character-sheet character-sheet-type 14 9 1 1 32 32))
(dict-set! x4-characters 'normal-14-10 
           (load-image character-sheet character-sheet-type 14 10 1 1 32 32))
(dict-set! x4-characters 'normal-14-11 
           (load-image character-sheet character-sheet-type 14 11 1 1 32 32))
(dict-set! x4-characters 'normal-14-12 
           (load-image character-sheet character-sheet-type 14 12 1 1 32 32))
(dict-set! x4-characters 'normal-14-13 
           (load-image character-sheet character-sheet-type 14 13 1 1 32 32))
(dict-set! x4-characters 'normal-14-14 
           (load-image character-sheet character-sheet-type 14 14 1 1 32 32))

(dict-set! x4-characters 'normal-15-0 
           (load-image character-sheet character-sheet-type 15 0 1 1 32 32))
(dict-set! x4-characters 'normal-15-1 
           (load-image character-sheet character-sheet-type 15 1 1 1 32 32))
(dict-set! x4-characters 'normal-15-2 
           (load-image character-sheet character-sheet-type 15 2 1 1 32 32))
(dict-set! x4-characters 'normal-15-3 
           (load-image character-sheet character-sheet-type 15 3 1 1 32 32))
(dict-set! x4-characters 'normal-15-4 
           (load-image character-sheet character-sheet-type 15 4 1 1 32 32))
(dict-set! x4-characters 'normal-15-5 
           (load-image character-sheet character-sheet-type 15 5 1 1 32 32))
(dict-set! x4-characters 'normal-15-6 
           (load-image character-sheet character-sheet-type 15 6 1 1 32 32))
(dict-set! x4-characters 'normal-15-7 
           (load-image character-sheet character-sheet-type 15 7 1 1 32 32))
(dict-set! x4-characters 'normal-15-8 
           (load-image character-sheet character-sheet-type 15 8 1 1 32 32))
(dict-set! x4-characters 'normal-15-9 
           (load-image character-sheet character-sheet-type 15 9 1 1 32 32))
(dict-set! x4-characters 'normal-15-10 
           (load-image character-sheet character-sheet-type 15 10 1 1 32 32))
(dict-set! x4-characters 'normal-15-11 
           (load-image character-sheet character-sheet-type 15 11 1 1 32 32))
(dict-set! x4-characters 'normal-15-12 
           (load-image character-sheet character-sheet-type 15 12 1 1 32 32))
(dict-set! x4-characters 'normal-15-13 
           (load-image character-sheet character-sheet-type 15 13 1 1 32 32))
(dict-set! x4-characters 'normal-15-14 
           (load-image character-sheet character-sheet-type 15 14 1 1 32 32))
