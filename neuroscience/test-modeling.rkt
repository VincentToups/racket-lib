#lang racket
(require (planet williams/simulation/simulation-with-graphics))
(require (planet williams/science/random-distributions))

 ;  Simulation Parameters
  (define end-time 720.0)
  (define n-pits 7)
  
  ;  Data collection variables
  (define total-ingots 0)
  (define wait-time #f)
  (define heat-time #f)
  (define leave-temp #f)
  
  ;  Model Definition
  (define random-sources (make-random-source-vector 4))
  
  (define furnace-set #f)
  (define furnace-temp 1500.0)
  (define pit #f)
  
  (define (scheduler)
    (let loop ((i 0))
      (schedule now (ingot i))
      (wait (random-exponential (vector-ref random-sources 0) 1.5))
      (loop (+ i 1))))
  
  (define-process (ingot i)
    (let* ((initial-temp
            (random-flat (vector-ref random-sources 1)
                         100.0 200.0))
           (heat-coeff
            (+ (random-gaussian
                (vector-ref random-sources 2) 0.05 0.01)
               0.07))
           (final-temp
            (random-flat (vector-ref random-sources 3)
                         800.0 1000.0))
           (current-temp (make-continuous-variable initial-temp))
           (arrive-time (current-simulation-time))
           (start-time #f))
      (with-resource (pit)
        (when (= (modulo i 100) 0)
          (accumulate (variable-history current-temp)))
        (set-variable-value!
         wait-time (- (current-simulation-time) arrive-time))
        (set-insert! furnace-set self)
        (set! start-time (current-simulation-time))
        (work/continuously
          until (>= (variable-value current-temp) final-temp)
          (set-variable-dt!
           current-temp
           (* (- furnace-temp (variable-value current-temp))
              heat-coeff)))
        (set-variable-value!
         heat-time (- (current-simulation-time) start-time))
        (set-variable-value!
         leave-temp (variable-value current-temp))
        (set-remove! furnace-set self))
      (when (variable-history current-temp)
        (write-special (history-plot
                        (variable-history current-temp)
                        (format "Ingot ~a Temp History" i)))
        (newline))
      (set! total-ingots (+ total-ingots 1))))
  
  (define (stop-sim)
    (printf "Report after ~a Simulated Hours - ~a Ingots Processed~n"
            (current-simulation-time) total-ingots)
    (printf "~n-- Ingot Waiting Time Statistics --~n")
    (printf "Mean Wait Time        = ~a~n"
            (variable-mean wait-time))
    (printf "Variance              = ~a~n"
            (variable-variance wait-time))
    (printf "Maximum Wait Time     = ~a~n"
            (variable-maximum wait-time))
    (printf "~n-- Ingot Heating Time Statistics --~n")
    (printf "Mean Heat Time        = ~a~n"
            (variable-mean heat-time))
    (printf "Variance              = ~a~n"
            (variable-variance heat-time))
    (printf "Maximum Heat Time     = ~a~n"
            (variable-maximum heat-time))
    (printf "Minimum Heat Time     = ~a~n"
            (variable-minimum heat-time))
    (printf "~n-- Final Temperature Statistics --~n")
    (printf "Mean Leave Temp       = ~a~n"
            (variable-mean leave-temp))
    (printf "Variance              = ~a~n"
            (variable-variance leave-temp))
    (printf "Maximum Leave Temp    = ~a~n"
            (variable-maximum leave-temp))
    (printf "Minimum Leave Temp    = ~a~n"
            (variable-minimum leave-temp))
    (write-special (history-plot
                    (variable-history leave-temp)
                    "Final Temperature Histogram"))
    (newline)
    (printf "~n-- Furnace Utilization Statistics --~n")
    (printf "Mean No. of Ingots    = ~a~n"
            (variable-mean (set-variable-n furnace-set)))
    (printf "Variance              = ~a~n"
            (variable-variance (set-variable-n furnace-set)))
    (printf "Maximum No. of Ingots = ~a~n"
            (variable-maximum (set-variable-n furnace-set)))
    (printf "Minimum No. of Ingots = ~a~n"
            (variable-minimum (set-variable-n furnace-set)))
    (write-special (history-plot
                    (variable-history (set-variable-n furnace-set))
                    "Furnace Utilization History"))
    (newline)
    (stop-simulation))
  
  (define (initialize)
    (set! total-ingots 0)
    (set! wait-time (make-variable))
    (set! heat-time (make-variable))
    (set! leave-temp (make-variable))
    (set! pit (make-resource n-pits))
    (set! furnace-set (make-set))
    (accumulate (variable-history (set-variable-n furnace-set)))
    (tally (variable-statistics wait-time))
    (tally (variable-statistics heat-time))
    (tally (variable-statistics leave-temp))
    (tally (variable-history leave-temp))
    (schedule (at end-time) (stop-sim))
    (schedule (at 0.0) (scheduler)))
  
  (define (run-simulation)
    (with-new-simulation-environment
      (initialize)
      (start-simulation)))
  
  (run-simulation)