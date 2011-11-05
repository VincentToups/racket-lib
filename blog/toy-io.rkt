#lang racket

(require racket/gui
         (only-in srfi/13 string-trim-both))

(define (string-contains-newline? str)
  (let loop ((chars (string->list str)))
    (match chars 
      [(list) #f]
      [(cons #\newline rest) #t]
      [(cons x rest)
       (loop rest)])))

(define (string-up-to-newline str)
  (let loop ((chars (string->list str))
             (acc '()))
    (match chars 
      [(list) (list->string (reverse acc))]
      [(cons #\newline rest) (list->string (reverse acc))]
      [(cons x rest) 
       (loop rest (cons x acc))])))

(define input-text% 
  (class text% 
    (field (command #f))
    (define/public (text-substring start len)
      (let loop ((acc '())
                 (i 0))
        (if (< i len) (loop (cons (send this get-character (+ start i)) acc) (+ i 1))
            (list->string (reverse acc)))))
    (define/augment (after-insert start len) 
      (let ((string (send this text-substring start len)))
        (if (string-contains-newline? string) 
            (let ((a-command (send this text-substring 2 (+ start len))))
              (set! command (string-up-to-newline (string-trim-both a-command)))
              (send this erase)
              (send this insert "> "))
            #f)))
    (define/public (grab-command) 
      (let ((c command))
        (set! command #f)
        c))
    (super-new)
    (send this insert "> ")))

(define (setup-window)
  (let* ((frame (new frame% 
                     [label "io toy"]
                     [width 640]
                     [height 480]))
         (pane (new vertical-pane% 
                    [min-width 640]
                    [min-height 480]
                    [parent frame]))
         (output-text (new text%))
         (output (new editor-canvas% 
                      [parent pane]
                      [style '(no-hscroll)]
                      [editor output-text]
                      [min-width 640]
                      [min-height 400]))
         (input-text (new input-text%))
         (input (new editor-canvas%
                     [parent pane]
                      [style '(no-hscroll)]
                      [editor input-text]
                      [min-width 640]
                      [min-height 40]))
         (io (lambda (input)
               (match input
                 [(? string? x) (send output-text insert input)]
                 ['clear (send output-text erase)]
                 ['fetch-command
                  (let loop 
                    ((command (send input-text grab-command)))
                    (yield)
                    (if command command (loop (send input-text
                                                    grab-command))))]))))
    (send frame show #t)
    io))

(define (run-io action) 
  (let ((io (setup-window)))
    (action io)))


(define (clear io) (io 'clear))
(define (get-command io) (io 'fetch-command))
(define (insert text) (lambda (io) (io text)))

(provide run-io clear get-command insert)