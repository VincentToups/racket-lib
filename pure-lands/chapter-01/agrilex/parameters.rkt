#lang racket

(require pure-lands/utilities/partial-application
		 pure-lands/utilities/ring-functions)

(define (membership-fun set)
  (lambda (x)
	(let loop
		((set set))
	  (match set
		[(list) #f]
		[(cons hd tl)
		 (if (equal? hd x) #t
			 (loop tl))]))))

(define stamina-draining-actions
  '(till plant water harvest))

(define stamina-draining-action?
  (membership-fun stamina-draining-actions))

(define (stamina-cost what)
  (match what
    ['till -4]
    ['plant -4]
    ['water -3]
    ['harvest  -4]
    ['food 2]
	['snack 2]
	['meal 4]
	['zesty-feast 10]))

(define weed-threshold 5)

(define (crop-maturation-time c)
  (+ 4 (match c
		 [(or 'e 'a 'i 'o 'n 'r 't 'l 's 'u) 1]
		 [(or 'd 'g) 2]
		 [(or 'b 'c 'm 'p) 3]
		 [(or 'f 'h 'v 'w 'y) 4]
		 [(or 'k) 5]
		 [(or 'j 'x) 8]
		 [(or 'q 'z) 10])))

(define (crop-time-till-seed c)
  (+ 4 (crop-maturation-time c)))

(define (crop-yield c)
  (+ 3
	 (match c
	   ['e 12]
	   [(or 'a 'i) 9]
	   ['o 8]
	   [(or 'n 'r 't) 6]
	   [(or 'l 's 'u 'd) 4]
	   ['g 3]
	   [(or 'b 'c 'm 'p 'f 'h 'v 'w 'y) 2]
	   [(or 'k 'j 'x 'q 'z) 1])))

(define mutation-rate 0.1)

(define (equal-to-one-of? thing whats)
  (match whats 
	[(list) #f]
	[(cons (? (partial< equal? thing)) rest)
	 #t]
	[(cons _ rest)
	 (equal-to-one-of? thing rest)]))


(define letters
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define letter-ring 
  (ring-over letters))

(define letter? (membership-fun letters))

(define hoes
  '(hoe fancy-hoe steam-hoe))


(define hoe? (membership-fun hoes))
(define (hoe-bonus hoe)
  (if (not (hoe? hoe)) (error (format "Can't calculate the hoe bonus of a non-hoe object (~a)." hoe))
	  (match hoe
		['hoe 1]
		['fancy-hoe 2]
		['steam-hoe 3])))

(define planters
  '(planter fancy-planter steam-planter))

(define (planter-bonus planter)
  (if (not (planter? planter?))
	  (error (format "Can't calculate the planter bonus of a non-planter object (~a)." planter))
	  (match planter
		['planter 1]
		['fancy-planter 2]
		['steam-planter 3])))

(define planter? (membership-fun planters))

(define harvesters
  '(scythe fancy-scythe combine))

(define harvester? (membership-fun harvesters))

(define (harvester-bonus harvest)
  (if (not (harvester? harvest))
	  (error (format "Can't calculate the harvest bonus of a non-harvester object (~a)." harvest))
	  (match harvest
		['scythe 1]
		['fancy-scythe 2]
		['combine 3])))

(define foods
  '(food snack meal zesty-feast))

(define food? (membership-fun foods))

(define items
  (append hoes planters harvesters foods))

(define item? (membership-fun items))


(provide stamina-cost weed-threshold 
         crop-maturation-time 
         crop-time-till-seed 
         crop-yield mutation-rate
         letters
         items
         hoes
         planters
         foods
         item?
         letter?
         food?
         hoe?
         harvester?
         planter?
         harvester-bonus
         planter-bonus
         hoe-bonus
		 stamina-draining-actions
		 stamina-draining-action?
		 letter-ring
         )
