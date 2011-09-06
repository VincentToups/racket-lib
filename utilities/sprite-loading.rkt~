#lang racket

(require racket/gui
         racket/class
         racket/dict
         racket/match
         )

(define (load-image filename type-sigil . args)
  (match args
    [(list)
     (let ((bitmap (make-object bitmap% 1 1)))
       (send bitmap
             load-file
             filename
             type-sigil)
       bitmap)]
    [(list x y w h)
     (sub-bitmap-of (load-image filename type-sigil) x y w h)]
    [(list x y w h tile-w tile-h)
     (sub-bitmap-of (load-image filename type-sigil)
                    (* x tile-w)
                    (* y tile-h)
                    (* w tile-w)
                    (* h tile-h))]
    [(list x y w h tile-w tile-h x-offset y-offset)
     (sub-bitmap-of (load-image
                     filename type-sigil
                     (+ (* x tile-w) x-offset)
                     (+ (* y tile-h) y-offset)
                     (* w tile-w)
                     (* h tile-h)))]))

(define (load-png filename)
  (load-image filename 'png/alpha))

(define (sub-bitmap-of bitmap x y w h)
  (let* ((target (make-object bitmap% w h (not (send bitmap is-color?))
                              #t))
         (target-dc (new bitmap-dc% [bitmap target])))
    (send target-dc draw-bitmap-section bitmap 0 0 x y w h)
    target))

(define (copy-bitmap bitmap)
  (sub-bitmap-of bitmap 0 0 (send bitmap get-width) (send bitmap get-height)))

(define image-cache (make-hash))
(define (load/cache-image filename type-sigil . args)
  (match args
    [(list) 
     (let ((result (dict-ref image-cache (list filename type-sigil) #f)))
       (if result result
           (let ((bitmap (load-image filename type-sigil)))
             (dict-set! image-cache (list filename type-sigil) bitmap)
             bitmap)))]
    [(list x y w h)
     (dict-ref image-cache (list filename type-sigil x y w h)
               (lambda ()
                 (let* ((source (load-image filename type-sigil))
                        (image (sub-bitmap-of source x y w h)))
                   (dict-set! image-cache (list filename type-sigil x y w h) image)
                   image)))]
    [(list x y w h tile-w tile-h)
     (load/cache-image filename  type-sigil
                       (* x tile-w)
                       (* y tile-h)
                       (* w tile-w)
                       (* h tile-h))]
    [(list x y w h tile-w tile-h x-offset y-offset)
     (load/cache-image filename type-sigil
                       (+ x-offset (* x tile-w))
                       (+ y-offset (* y tile-h))
                       (* w tile-w)
                       (* h tile-h))]))

(define (scale-bitmap bitmap . args)
  (match args
    [(list scale)
     (scale-bitmap bitmap scale scale)]
    [(list scale-x scale-y)
     (let* ((w (send bitmap get-width))
            (h (send bitmap get-height))
            (new-w (* scale-x w))
            (new-h (* scale-y h))
            (target (make-object bitmap% new-w new-h #f #t))
            (target-dc (new bitmap-dc% [bitmap target])))
       (send target-dc set-smoothing 'unsmoothed)
       (send target-dc set-scale scale-x scale-y)
       (send target-dc set-smoothing 'unsmoothed)
       (send target-dc draw-bitmap bitmap 0 0)
       (send target-dc set-smoothing 'unsmoothed)
       target)]))

(define (show-bitmap bitmap . args)
  (match args
    [(list scale)
     (let* ((scaled-bitmap (scale-bitmap bitmap scale))
            (f (new frame% [width (send scaled-bitmap get-width)]
                    [height (send scaled-bitmap get-height)]
                    [label "Bitmap Preview"]))
            (draw
             (lambda (self dc)
               (send dc set-smoothing 'unsmoothed)
               (send dc draw-bitmap scaled-bitmap 0 0)))
            (c (new canvas% [parent f]
                    [paint-callback draw])))
       (send f show #t)
       f)]
    [(list)
     (show-bitmap bitmap 1)]))

(define (clear-image-cache . args)
  (match args
    [(list) (set! image-cache (make-hash))]
    [(cons item (list))
     (dict-remove! image-cache item)]
    [(cons item items)
     (dict-remove! image-cache item)
     (apply clear-image-cache items)]))

(provide load-image load-png sub-bitmap-of load/cache-image
         scale-bitmap show-bitmap clear-image-cache)
