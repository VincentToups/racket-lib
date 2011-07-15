#lang slideshow

(require 
 slideshow/pict
 slideshow/code
 functional/monads
 racket/gui
 ;(except-in roguelikes/turtles-monad circle rectangle)
 (prefix-in pg: utilities/planar-geometry)
 (prefix-in mts: racketcon/monadic-turtles)
 (prefix-in mt: racketcon/monadic-turtle))

(slide #:title "Purely Functional Turtle Graphics with Monads"
       #:layout 'center
 (mts:turtles->pict 
        (mts:turtles-let* 
         ((new-pos (mts:jump-to 150 150))
          (len     (mts:set-local 'len 2))
          (new-pos (mts:n-times 200
                    (mts:turtles-let* 
                      ((len (mts:get-local 'len))
                       (p   (mts:move len))
                       (r   (mts:turn (/ pi 1.99)))
                       (len (mts:set-local 'len (+ len 2))))
                      (mts:turtles-return p))
                    )))
         (mts:turtles-return new-pos)))
 (t "A monad tutorial!"))

(slide #:title "What we want to accomplish"
       (item "Understand monads.")
       (item "Use a monad to represent a purely functional turtle graphics system.")
       (item "Justify using a monadic solution by extending the monad to add powerful new features to our system."))

(slide #:title "Turtle Graphics Primer"
       (item "Turtle Graphics use a very clever turtle with a pen to draw pictures.")
       (item "A program is a series of commands to the turtle which implicitly modify its state. Eg (in pseudo-code): ")
       (item (subitem "move 10")
             (subitem "turn pi/2")
             (subitem "move 15")
             (subitem "turn pi/4")
             (subitem "move 20"))
       (item "Produces:")
       (item (mts:turtles->pict
              (mts:turtles-let* 
                     ((p (mts:jump-to 25 25))
                      (p (mts:move 10))
                      (r (mts:turn (/ pi 2)))
                      (p (mts:move 15))
                      (r (mts:turn (/ pi 4)))
                      (p (mts:move 20)))
                     (mts:turtles-return p))
              '(width . 50) '(height . 50))))
             
(slide #:title "Turtle Observations"
       (item "Obviously stateful.")
       (item "State includes:"
             (subitem "Position (a pair of numbers)")
             (subitem "Heading (a single number in radians)")
             (subitem "a \"pen-down\" flag, pen color, memory, etc..."))
       (item "Supposing we want to \"wear the hair shirt\" and simulate our turtle without side effects.  How?"))

(current-code-font (make-object font% 12 'swiss))

(slide #:title "Sewing the Hair Shirt"
       (item "Turtle Representation, an association list"
             (subitem (code (define (make-turtle) (list (cons 'pos (point 150 150))
                                                        (cons 'facing 0)
                                                        (cons 'pen-state 'down)))
                            )))
       (item "Each \"command\" is a function which accepts a state and returns..?")
       (item "Well, at least a new state.")
       (item "We'll also let turtle functions return a value in addition to a new state.")
       (item "That is, commands or turtle functions return a doublet of values, the first an arbitrary value and the second a new state.")
       ) 

(slide #:title "Useful Operations I"
  (item (code (defstruct doublet (a b))))
  (item (code (define (move distance)
                 (lambda (turtle-state)
                   (let* ((pos (dict-ref turtle-state 'pos))
                         (face (dict-ref turtle-state 'facing))
                         (new-pos (vector+ pos
                                           (vector-from-dir-and-len 
                                            distance
                                            face))))
                     (doublet 
                      new-pos
                      (dict-set turtle-state 'pos new-pos)))))))
  (item (code (define (turn amount)
                (lambda (turtle-state)
                  (let* ((face (dict-ref turtle-state 'facing))
                         (new-facing (+ facing amount)))
                    (doublet new-facing
                             (dict-set turtle-state 'facing new-facing))))))))
(slide #:title "Useful Operations II"
  (item (code (define (turtle-return item) 
                (lambda (turtle-state)
                  (doublet item turtle-state)))))
  (item (code (define (turtle-bind turtle-function
                                   turtle-function-producer)
                (lambda (turtle-state)
                  (match (turtle-function turtle-state)
                    [(doublet returned-val new-state)
                     (let ((new-turtle-fun (turtle-function-producer returned-val)))
                       (new-turtle-fun new-state))]))))))

(slide #:title "In the Monads of Madness"
       (item "Turtle functions, along with turtle-return and turtle-bind, form a monad!")
       (item "What is a monad?")
       (item "A collection of data types which satisfy certain constraints, like:"
             (subitem "There exist operations `bind` and `return` and types `monadic values,` `monadic-functions` and `values` for which:")
             (subitem (code (bind a return) -> a))
             (subitem (code (bind (return a) f) -> (f a)))
             (subitem "Where `a` is a monadic value.")
             ))

(slide #:title "Yeah, but what is a monad?"
       (item "Ok, ok - a monad is just a way of extending the idea of variable binding. Consider that:")
       (item (code (let* ((x 10)
                          (y (+ x 11)))
                     (+ x y))))
       (item "Can be expanded as:")
       (item (code
              (define (simple-bind x f) (f x))
              (simple-bind 10 (lambda (x)
                         (simple-bind (+ x 11) 
                               (lambda (y)
                                 (+ x y)))))))
       (item "above, `simple-bind` is just function application")
       (item "but what if we replace \"simple-bind\" with a \"richer\" operation?"))

(slide #:title "Imagine..."
       (item "A special form, `turtle-let*`, such that:")
       (item (code 
              (turtle-let* ((new-facing (turn (/ pi 2)))
                            (new-position
                             (move 10)))
                           (turtle-return new-position))))
       (item "Expands to")
       (item (code
              (turtle-bind (turn (/ pi 2))
                           (lambda (new-facing)
                             (turtle-bind
                              (move 10)
                              (lambda (new-position)
                                (turtle-return new-position)))))))
       (item "What is the value of this expression?"))

(slide #:title "turtle-let* constructs turtle functions"
       (item "Hint:")
       (item (code (((turtle-let* ((new-facing (turn (/ pi 2)))
                                                       (new-position
                                                        (move 10)))
                                                      (turtle-return new-position)))
                            (make-turtle))))
       (item "Is a valid expression, and results in a turtle-state with a position of (-10 0) and an facing of pi/2")
       (item "N.B.: the \"turtle monad\" is really just the state monad."))

(slide #:title "Actually drawing things."
       (ht-append (code
        (define (make-turtle)
          (list
           (cons 'pos (point 150 150))
           (cons 'facing 0)
           (cons 'things-to-draw '())
           (cons 'motion-function add-line-between)))
        
        (define (add-line-between p1 p2)
          (turtle-let* 
           ((things (get-val 'things-to-draw '()))
            (things (set-val 'things-to-draw (cons (line-segment p1 p2) things))))
           (turtle-return things))))
        (blank 10 300)
        (rectangle 10 300)
        (blank 10 300)
        (code
        (define (turtle->pict fun width height)
          (let* ((bitmap (make-object bitmap% width
                           height
                           #f
                           #f))
                 (dc (new bitmap-dc% [bitmap bitmap]))
                 (final-state (fun (make-turtle)))
                 (things (dict-ref (doublet-b final-state) 'things-to-draw '())))
            (let loop ((things things))
              (match things
                [(list) (bitmap->pict bitmap)]
                [(cons thing things)
                 (draw-shape dc thing)
                 (loop things)])))))))
       
(slide #:title "Pretty pictures!"
       (item 
        (code 
         (turtle->pict (turtle-let* ((p (jump-to 150 150))
                              (_ (set-val 'len 2))
                              (_ (n-times 250 
                                          (turtle-let* 
                                           ((a (turn (/ pi 1.99)))
                                            (l (get-val 'len))
                                            (np (move l))
                                            (_ (set-val 'len (+ l 2))))
                                           (turtle-return np)))))
                             (turtle-return _)))))
       (item (mt:turtle->pict (mt:turtle-let* ((p (mt:jump-to 150 150))
                              (_ (mt:set-val 'len 2))
                              (_ (mt:n-times 250 
                                          (mt:turtle-let* 
                                           ((a (mt:turn (/ pi 1.99)))
                                            (l (mt:get-val 'len))
                                            (np (mt:move l))
                                            (_ (mt:set-val 'len (+ l 2))))
                                           (mt:turtle-return np)))))
                             (mt:turtle-return _)))))

(slide #:title "Extending the Monad to Support Parallel Turtles"
       (item "Ordinary Turtle Graphics doesn't do symmetric images well.")
       (item "Wouldn't it be nice to draw a `Y` by saying:"
             (subitem "face north")
             (subitem "move 10")
             (subitem "turn 45 degrees left AND right")
             (subitem "move 7"))
       (item "We'll create a new turtle monad capable of step 3 above."))

(slide #:title "Extending the State Monad"
       (item "We want to create a bind operation that handles multiple turtles, with their own state, AND a shared, global state (for the drawing).")
       (item "A good approach is to consider turtle commands.")
       (item "They should accept a pair of states, a local and global state, and return at least a possibly modified global state.")
       (item "They also should return a list of multiple outcomes."))

(slide #:title "Some code"
       (item "Initial turtle state:")
       (item (code (define (make-turtles)
                     (doublet
                      ; local state
                      (list (cons 'pos (point 150 150))
                            (cons 'facing 0)
                            (cons 'helicity 1)
                            (cons 'motion-function add-line-pts)
                            (cons 'scaling 1))
                      ; global state
                      (list (cons 'things-to-draw '()))))))
       (item "Return, the simplest turtles command.")
       (item (code (define (turtles-return item)
                     (lambda (state-doublet)
                       (match state-doublet
                         [(doublet local-state global-state)
                          (doublet (list (doublet item local-state)) global-state)]))))))

(slide #:title "Deep breath: turtles-bind"
       (code (define (turtles-bind turtlesf turtlesf-prod)
  (lambda (state-doublet)
    (match (turtlesf state-doublet)
      [(doublet local-doublets global-state)
       (let loop ((local-doublets local-doublets)
                  (global-state global-state)
                  (local-doublets-out '()))
         (match local-doublets
           [(list) (doublet (reduce append (reverse local-doublets-out)) global-state)]
           [(cons local-doublet local-doublets)
            (match local-doublet
              [(doublet val local-state)
               (let ((new-turtle-fun (turtlesf-prod val)))
                 (match (new-turtle-fun (doublet local-state global-state))
                   [(doublet sub-local-doublets
                             global-state)
                    (loop local-doublets global-state 
                          (cons sub-local-doublets local-doublets-out))]))])]))])))))
       
(slide #:title "Things get interesting."
       (item "Now something to set local variables in parallel.")
       (item (code (define (split-set symbol . vals)
                     (lambda (state-doublet)
                       (match state-doublet
                         [(doublet local-state global-state)
                          (doublet (map
                                    (lambda (val)
                                      (doublet val (dict-set local-state symbol val)))
                                    vals)
                                   global-state)])))))
       (item "split-set takes a symbol and a value and returns a doublet representing the result of setting the symbol to ALL of the values.")
       (item "Subsequent \"turtles\" commands are then threaded, by turtles-bind, over all the possible local states, while global state is accumulated over all turtles."))

(define (spiraling-shape n irregularity)
                (mts:turtles-let* 
                 ((len (mts:set-local 'len 2))
                  (final-pos
                   (mts:n-times n 
                            (mts:turtles-let* 
                             ((a (mts:turn (* (/ pi 2) irregularity)))
                              (len (mts:get-local 'len))
                              (np (mts:move len))
                              (len (mts:set-local 'len (+ len 2))))
                             (mts:turtles-return np)))))
                 (mts:turtles-return final-pos)))

(slide #:title "An example:"
       (item (ht-append (code 
              (define (spiraling-shape n irregularity)
                (turtles-let* 
                 ((len (set-local 'len 2))
                  (final-pos
                   (n-times n 
                            (turtles-let* 
                             ((a (turn (* (/ pi 2) irregularity)))
                              (len (get-local 'len))
                              (np (move len))
                              (len (set-local 'len (+ len 2))))
                             (m-return np)))))
                 (turtles-return final-pos))))
                        (blank 5 300)
                        (rectangle 10 300)
                        (blank 5 300)
                        (code (turtles->pict 
                               (turtles-let* 
                                ((pos (split-set 'pos 
                                                 (point 75 75)
                                                 (point 225 75)
                                                 (point 225 225)
                                                 (point 75 225)))
                                 (final-pos (spiraling-shape 40 1.1)))
                                (turtles-return final-pos))))))
       (item (mts:turtles->pict 
              (mts:turtles-let* 
               ((pos (mts:split-set 'pos 
                                    (pg:point 75 75)
                                    (pg:point 225 75)
                                    (pg:point 225 225)
                                    (pg:point 75 225)))
                (final-pos (spiraling-shape 40 1.1)))
               (mts:turtles-return final-pos)))))

(slide #:title "Conclusions"
       (item "Monadic computations facilitate elegant representations of purely functional code.")
       (item "Because we are already thinking monadically, it was easy to extend our turtles with parallel operations.")
       (item "Almost all the hard work is squeezed into getting the right bind operation.")
       (item "All this code is available at my github: https://github.com/VincentToups/racket-lib/tree/master/racketcon")
       (item "Thanks to: Triangle Functional Programmers, Konrad Hinson's (Clojure Monads), Jim Duey (Monad Tutorials in Clojure)."))


       


             
       
     
       
                
