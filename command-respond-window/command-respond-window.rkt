#lang racket

(require racket/gui
         functional/better-monads)

(define command-input% 
  (class text%
    (init-field [command-callback (lambda (command) #t)])
    (field (command #f))
    (define/public (get-command) 
      (if command 
          (let ((c command))
            (set! command #f)
            c)
          #f))
    (define/public (text-substring start len)
      (let loop ((acc '())
                 (i 0))
        (if (< i len) (loop (cons (send this get-character (+ start i)) acc) (+ i 1))
            (list->string (reverse acc)))))
    (define/augment (after-insert start len)
      (let ((string (send this text-substring start len)))
        (if (string-contains-newline? string) 
            (let ((a-command (send this text-substring 2 (+ start len))))
              (command-callback a-command)
              (set! command a-command)
              (send this erase)
              (send this insert "> "))
            #f)))
    (super-new)))

(define (string-contains-newline? str)
  (let loop 
    ((chars (string->list str)))
    (cond
      ((empty? chars) #f)
      ((equal? (first chars) #\newline) #t)
      (#t (loop (rest chars))))))

(define (make-command-respond-window label command-handler)
  (let* [(frame (new frame% [width 640]
                            [height 480]
                            [label label]))
         (pane (new vertical-pane% 
                    [min-height 480]
                    [min-width 480]
                    [parent frame]))
         (output-text (new text%))
         (output-editor (new editor-canvas% 
                           [parent pane]
                           [style (list 'no-hscroll)]
                           [editor output-text]
                           [label "Output:"]
                           [min-width 640]
                           [min-height 480]))
         (input-text (new command-input% [command-callback command-handler]))
         (command-region (new editor-canvas%  
                              [parent pane]
                              [style (list 'no-hscroll)]
                              [editor input-text]
                              [min-width 640]
                              [min-height 40]
                              [stretchable-height 40]
                              ))
         (interaction 
          (match-lambda 
            ['command
             (let loop ((command (send input-text get-command)))
               (yield)
               (if command command (loop (send input-text get-command))))
             ]
            ['clear (send output-text erase)]
            [(? string? x) (send output-text insert x)]))]
    (send frame show #t)
    (send input-text insert "> ")
    (send output-text insert "Output: ")
    interaction))

(define (io-bind command future)
  (lambda (io)
    (let ((value (command io)))
      ((future value) io))))

(define (io-return item)
  (lambda (io)
    item))

(define (io-plus i1 i2)
  (lambda (io) (i1 io) (i2 io)))

(define (read-command io)
  (io 'command))

(define (write-output text)
  (lambda (io)
    (io text)
    #f))

(define (clear io) (io 'clear))

(define io-monad (monad io-bind io-return io-plus #f))

(define (do-command-respond-window command)
  (let ((io (make-command-respond-window "" (lambda (x) x))))
    (command io)))

;(define interaction (make-command-respond-window "test" (lambda (x) x) ))

(provide make-command-respond-window do-command-respond-window); interaction)