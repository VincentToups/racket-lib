#lang racket

(require blog/toy-io)

(define (io-bind io-action io-action-producer)
  (lambda (io)
    (let* ((result (io-action io))
           (next-io-action
            (io-action-producer result)))
      (next-io-action io))))

(define (io-return value)
  (lambda (io)
    value))

(define exit-flagger 
  (io-bind get-command 
           (lambda (command)
             (if (equal? command "exit") (io-return 'exit-detected)
                 exit-flagger))))

(define (read-command str)
  (with-handlers ([exn:fail? (lambda (e) 'error)])
    (with-input-from-string 
     (string-append "(" str ")")
     (lambda () (read)))))

(define-syntax do-io 
  (syntax-rules (<-)
    [(do-io (id <- io-action) expr ...)
     (io-bind io-action 
              (lambda (id)
                (do-io expr ...)))]
    [(do-io io-action expr0 expr ...)
     (io-bind io-action 
              (lambda (_) 
                (do-io expr0 expr ...)))]
    [(do-io io-action) io-action]))


(define (repeat-char n char)
  (let loop ((n n)
             (acc '()))
    (if (<= n 0) (list->string acc)
        (loop (- n 1) (cons char acc)))))

(define symbolic-command
  (do-io 
   (cmd <- get-command)
   (io-return (read-command cmd))))

(define dispatch
  (do-io
   (c <- symbolic-command)
   (match c
     ['error 
      (do-io
       (insert "Error reading command string.")
       dispatch)]
     [(list 'stars (? number? n))
      (do-io 
       clear
       (insert (repeat-char n #\*))
       dispatch)]
     [(list 'exit) (io-return 'exit-detected)]
     [_ (do-io
         clear
         (insert "unknown command.")
         dispatch)])))
