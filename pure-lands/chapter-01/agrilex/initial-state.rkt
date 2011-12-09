#lang racket

(require pure-lands/utilities/nested-dicts)

(define initial-letters
  (>> 'f 2 'o 4 'd 2))

(define initial-plot
  (>> 'wet #f
      'planted #f
      'weeds 0
      'harvest #f
      'time-since-planting #f))

(define initial-state
  (>> 'stamina 6
      'turn 0
      'points 0
	  'weather 'clear
      'letters initial-letters
      'items '()
      'field '()
      'random-state
      #(1 2 3 4 5 6)))

(provide initial-state initial-plot)
