#lang racket

(require utilities/fancy-destructuring
         utilities/lists
         functional/point-free
         racket/dict)

(defn (step-state-machine ((:> as machine) state 'state))
  (dlet* (((:> do-what state
               otherwise '--otherwise) machine))
         (cond ((procedure? do-what) 
                (do-what machine))
               ((eq? 'unbound do-what)
                (if (eq? 'unbound otherwise) machine
                    (otherwise machine do-what)))
               (else
                (dict-set machine 'state do-what)))))

(define (step-state-machine-n-times machine n)
  (let loop ((machine machine)
             (c 0))
    (if (= c n) machine
        (loop (step-state-machine machine) 
              (+ c 1)))))

(define (step-state-machine-while machine pred)
  (let loop ((machine machine))
    (if (not (pred machine)) machine
        (loop (step-state-machine machine)))))

(define (step-state-machine-get machine element)
  (let ((new-machine (step-state-machine machine)))
    (list (dict-ref machine element) machine)))

(define (step-state-machine-n-times-acc machine n element)
  (let loop ((machine machine)
             (c 0)
             (acc '()))
    (if (= c n) (list (reverse acc) machine)
        (let ((new-machine (step-state-machine machine)))
          (loop new-machine (+ c 1) (cons (dict-ref new-machine element) acc))))))
        
(provide step-state-machine-n-times-acc
         step-state-machine-n-times
         step-state-machine-while
         step-state-machine)