#lang racket

(require
 pure-lands/chapter-01/agrilex/initial-state
 pure-lands/chapter-01/agrilex/state-commands
 pure-lands/utilities/dict-state-monad
 pure-lands/utilities/association-list-equality)

(define test-turn+ 
  (build
   (n <- get-turn)
   (turn+)
   (n-new <- get-turn)
   (state-return
	(if (not 
		 (= (+ n 1) n-new))
		(error "(turn+) failed to increment the turn by one.")
		'success))))

(test-turn+ initial-state)


(define test-item-adding 
  (build
   (add-item 'hoe)
   (items <- get-items)
   (if (not (equal? 
			 items
			 '((hoe . 1))))
	   (error "Adding a hoe failed!  You may have a thousand problems, but a hoe is one.")
	   (==> items))))

(test-item-adding initial-state)

(define test-item-subtracting
  (build 
   (add-item 'hoe)
   (add-item 'scythe)
   (add-item 'snack 10)
   (remove-item 'snack 3)
   (items <- get-items)
   (if
	(not (asc-equal? 
		  items
		  '((hoe . 1)
			(scythe . 1)
			(snack . 7))))
	(error "Item adding and subtracing fails!")
	(==> items))))


(test-item-subtracting initial-state)


(define test-item-subtracting-no-error
  (build 
   (add-item 'hoe)
   (add-item 'scythe)
   (add-item 'snack 10)
   (remove-item 'snack 11)
   (items <- get-items)
   (if
	(not (asc-equal? 
		  items
		  '((hoe . 1)
			(scythe . 1)
			(snack . 0))))
	(m-return (error "Item adding and subtracing fails!"))
	(==> items))))

(test-item-subtracting-no-error initial-state)

