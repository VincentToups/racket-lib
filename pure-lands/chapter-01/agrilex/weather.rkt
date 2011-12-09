#lang racket

(require 
 pure-lands/utilities/dict-state-monad
 pure-lands/utilities/partial-application
 pure-lands/chapter-01/agrilex/state-commands
 pure-lands/chapter-01/agrilex/parameters
 pure-lands/chapter-01/agrilex/initial-state
 pure-lands/chapter-01/agrilex/randomness)

(define tick-weather 
  (build
   (w <- (get 'weather))
   (match w
	 ['raining 
	  (build 
	   (r <- random-uniform)
	   (if (< r .3)
		   (set 'weather 'clear)
		   (set 'weather 'raining)))]
	 ['clear 
	  (build 
	   (r <- random-uniform)
	   (if (< r .3)
		   (set 'weather 'raining)
		   (set 'weather 'clear)))])))

(provide tick-weather)
