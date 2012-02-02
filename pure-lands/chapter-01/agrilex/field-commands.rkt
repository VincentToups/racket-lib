#lang racket

(require 
 pure-lands/utilities/dict-state-monad
 pure-lands/utilities/partial-application
 pure-lands/chapter-01/agrilex/state-commands
 pure-lands/chapter-01/agrilex/parameters
 pure-lands/chapter-01/agrilex/initial-state
 pure-lands/chapter-01/agrilex/randomness)

(define (plot-name-available? name)
  (build
   (r <- (get* 'field name))
   (==> (not r))))

(define (get-plot name)
  (build
   (maybe-plot <- (plot-name-available? name))
   (if maybe-plot
	   (error (format "Can't get the plot ~a because it doesn't exist." name))
	   (get* 'field name))))

(define (create-plot name)
  (build 
   (avail <- (plot-name-available? name))
   (if avail 
	   (set* 'field name initial-plot)
	   (error (format "Tried to create a plot where a plot already existed (at ~a)." name)))))

(define (reset-plot name)
  (build
   (avail <- (plot-name-available? name))
   (if (not avail)
	   (error (format "Can't reset a plot (~a) which doesn't yet exist." name))
	   (set* 'field name initial-plot))))

(define (reset/create-plot name)
  (set* 'field name initial-plot))

(define (assert-plot-exists name)
  (build
   (avail <- (plot-name-available? name))
   (if avail  
	   (==> (error (format "Plot name ~a doesn't exist, but an assertion required it." name)))
	   (==> #t))))

(define (watered? name)
  (build
   (assert-plot-exists name)
   (get* 'field name 'wet)))

(define (wet name)
  (build
   (assert-plot-exists name)
   (set* 'field name 'wet #t)))

(define (dry name)
  (build 
   (assert-plot-exists name)
   (set* 'field name 'wet #f)))

(build/define 
 plot-names
 (f <- (get 'field))
 (==> (map first f)))

(build/define 
 (for-each-plot command)
 (names <- plot-names)
 (build/let 
  loop 
  ((names names))
  (match names
	[(list) state-true]
	[(cons name names)
	 (build
	  (command name)
	  (loop names))])))


(build/define 
 handle-weather
 (w <- (get 'weather))
 (for-each-plot 
  (lambda 
   (name)
   (if (equal? w 'raining)
	   (wet name)
	   do-nothing))))

(build/define (plot-gone-to-seed? name)
			  (assert-plot-exists name)
			  (crop <- (get* 'field name 'planted))
			  (ticks <- (get* 'field name 'time-since-planting))
			  (crop-flag := (if (not crop) #f
								(>= ticks (crop-time-till-seed crop))))
			  (weeds <- (get* 'field name 'weeds))
			  (==> (or crop-flag
					   (>= weeds weed-threshold))))

(define (incr-letter harvest letter)
  (let* ((n (dict-ref harvest letter #f))
		 (n+ (if n (+ n 1) 1)))
	(dict-set harvest letter n+)))

(build/define (calculate-harvest crop)
			  (n := (crop-yield crop))
			  (let-build
			   loop
			   ((n n)
				(harvest '()))
			   (mutate-draw <- random-uniform)
			   (build/cond
				((< mutate-draw mutation-rate)
				 (direction-draw	<- random-uniform)
				 (direction			:= (if (> direction-draw .5) 'right 'left))
				 (letter			:= (letter-ring crop direction))
				 (loop (- n 1)
					   (incr-letter harvest letter)))
				(#t
				 (loop (- n 1)
					   (incr-letter harvest crop))))))

(build/define (update-harvest plot-name)
			  (assert-plot-exists plot-name)
			  (crop <- (get* 'field plot-name 'planted))
			  (build/when 
			   crop 
			   (ticks <- (get* 'field plot-name 'time-since-planting))
			   (build/when
				(>= ticks (crop-maturation-time crop))
				(harvest <- 
						 (calculate-harvest crop))
				(set* 'field plot-name 'harvest harvest))))

(build/define (tick-plot name)
			  (assert-plot-exists name)
			  (build/when 
			   (<- (state-and
					(get* 'field name 'planted)
					(watered? name)))
			   (transform* 'field name 'time-since-planting
						   (>partial + 1))
			   (set* 'field name 'wet #f)
			   (update-harvest)
			   (build/when (<- (plot-gone-to-seed? name))
						   (reset-plot name))))



(provide 
 plot-name-available?
 get-plot
 create-plot
 reset-plot
 reset/create-plot
 assert-plot-exists
 watered?
 wet
 dry
 plot-names
 for-each-plot
 handle-weather
 plot-gone-to-seed?
 incr-letter
 calculate-harvest
 update-harvest
 tick-plot)