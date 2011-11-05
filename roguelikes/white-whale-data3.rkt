#lang racket
(require utilities/proletariat
         functional/point-free)

(define (dip-into dict keys fun)
  (match keys
    [(? symbol? key)
     (let ((val (dict-ref dict key #f)))
       (dict-set dict key (fun val)))]
    [(list key)
     (dip-into dict key fun)]
    [(cons key rest)
     (dip-into dict key (lambda (inner-d)
                          (dip-into inner-d rest fun)))]))

(define dict-dip dip-into)

(define/class cursable (object) 'curse-status 'uncursed)
(define/class propertied (object) 'properties (make-immutable-hash '()))

(define-multimethod (set-property pd key val) :: (class-name pd))
(define-multimethod (dip-property pd key val) :: (class-name pd))

(define-method (set-property pd key val) :: propertied
  (dip-into pd 'properties (lambda (_) (dict-set _ key val))))

(define-method (dip-property pd key fun) :: propertied
  (dip-into pd '(properties key) fun))

(define/class opaque (object))
(define/class passable (object))
(define (impassable? x) (not (passable? x)))
(define (transparent? x) (not (opaque? x)))

(define/class tile (object) 'things '() 'entities '())
(define/class floor (tile))
(define/class wall (opaque tile))
(define/class hall (floor))
(define/class fluid (tile) 'depth 7)
(define/class water (fluid))
(define/class lava (fluid))
(define/class poison (fluid))
(define/class exit (tile) 'to-location #f)
(define/class stairs (opaque exit))
(define/class up-stairs (stairs))
(define/class down-stairs (stairs))
(define/class sideways-exit (exit))

(define/class limited-integer (object) 'value 0 'max 0)
(define-method (as li n) :: #(limited-integer number)
  (dict-ref li 'value))
(define-multimethod (set-value/max o to) :: (class-name o))
(define-method (set-value/max o to) :: limited-integer
  (adjust o 'max to 'value to))
(define-multimethod (lim+ li n) :: (class-name li))
(define-multimethod (lim- li n) :: (class-name li))
(define-method (lim+ li n) :: limited-integer
  (let ((max-val (dict-ref li 'max)))
    (dict-dip li 'value (lambda (x) (max 0 (min max-val (round (+ x n))))))))
(define-method (lim- li n) :: limited-integer
  (lim+ li (- n)))

(define (limint v m)
  (adjust limited-integer 'max m 'value v))

;;; Traits
(define/class trait (object))
(define/class modifiable (object trait) 'modifier limited-integer)
(define/class use-trackable (object trait) 'use-count 0)
(define/class melee-weapon-trait (object trait))
(define/class monster-trait (object trait))
(define/class shield-trait (object trait))
(define/class stackable (object trait) 'count 1)
(define/class charge-bearing (object shield-trait melee-weapon-trait) 'charge #f)
(define/class hunger-modifying (object shield-trait) 'hunger-factor #f)
(define/class breakable (object melee-weapon-trait) 'break-probability  #f)
(define/class wall-destroying (object melee-weapon-trait))
(define/class supernatural (object melee-weapon-trait monster-trait))
(define/class cybernetic (object melee-weapon-trait monster-trait))
(define/class self-damaging (object melee-weapon-trait) 'reciprocal-damage-percentage 0.1)
(define/class grasping (object monster-trait))

(define-multimethod (track-use thing) :: (class-name thing))
(define-method (track-use u) :: use-trackable 
  (adjust u 'use-count (depending-on (use-count) (+ 1 use-count))))

(define-method (track-use c) :: charge-bearing
  (adjust (call-next-method) 'charge (depending-on (charge)
                                  (- charge 1))))

;(prefer-method track-use 'charge-bearing 'use-trackable)

(define-preference track-use :: (< charge-bearing use-trackable))
          

(define/class item (cursable))
(define/class melee-weapon (item propertied modifiable use-trackable) 'attack #f 'name #f)
(define/class shield (item propertied modifiable use-trackable) 'defense #f 'name #f)
(define/class ammunition (item propertied use-trackable) 'attack #f 'name #f)

(define-multimethod (traits o) :: (class-name o))
(define-method (traits o) :: object
  (let ((parents (dict-keys (ancestors (class-name o)))))
    (filter (partial< derived? 'trait) parents)))

(define-multimethod (shield-traits o) :: (class-name o))
(define-method (shield-traits o) :: object
  (let ((parents (dict-keys (ancestors (class-name o)))))
    (filter (partial< derived? 'shield-trait) parents)))

(define-multimethod (monster-traits o) :: (class-name o))
(define-method (monster-traits o) :: object
  (let ((parents (dict-keys (ancestors (class-name o)))))
    (filter (partial< derived? 'monster-trait) parents)))

(define-multimethod (melee-weapon-traits o) :: (class-name o))
(define-method (melee-weapon-traits o) :: object
  (let ((parents (dict-keys (ancestors (class-name o)))))
    (filter (partial< derived? 'melee-weapon-trait) parents)))


;;; Shields 
(define/class forearm (shield) 'defense 2 'name "forearm")
(define/class loose-paneling (shield) 'defense 3 'name "loose paneling")
(define/class bulkhead-fragment (shield) 'defense 5 'name "bulkhead fragment")
(define/class energy-shield (shield charge-bearing) 'defense 2 
  'name "energy shield"
  'charge 25)
(define/class medical-system (shield hunger-modifying) 'defense 2 'hunger-factor 0.5)
(define/class defensive-symbiote (shield hunger-modifying) 'defense 12 'hunger-factor 2.0)

;;; Melee Weapons
(define/class bare-hands (melee-weapon) 'attack 1 'name "bare hands")
(define/class suit-gauntlet (melee-weapon) 'attack 3 'name "suit gauntlet")
(define/class heavy-mechanism (melee-weapon) 'attack 5 'name "heavy mechanism")
(define/class memory-crystal-shard (melee-weapon breakable self-damaging) 'attack 8 'break-probability 0.1 'name "memory crystal shard")
(define/class topiary-fractal (melee-weapon breakable wall-destroying) 
  'name "topiary fractal"
  'attack 3 
  'break-probability 0.1)
(define/class energy-sword (melee-weapon charge-bearing) 'attack 2 'name "energy sword" 'charge 25)
(define/class wire-bundle (melee-weapon) 'attack 5 'name "wire bundle")
(define/class electric-prod (melee-weapon cybernetic) 'attack 5 'name "electric prod")
(define/class ouija-shiv (melee-weapon supernatural) 'attack 4 'name "ouija shiv")

(define/class bullets (ammunition) 'count 12)
(define/class pulse-bullets (ammunition) 'count 12)
(define/class swap-bullets (ammunition) 'count 12)
(define/class paint-bullets (ammunition) 'count 12)
(define/class stasis-bullets (ammunition) 'count 12)
(define/class bind-bullets (ammunition) 'count 12)
(define/class dilation-bullets (ammunition) 'count 12)
(define/class inertia-bullets (ammunition) 'count 12)
(define/class push-bullets (ammunition) 'count 12)

(define/class food (item) 'recovery-amount #f)
(define/class meal (food) 'recovery-amount 100)
(define/class snack (food) 'recovery-amount 50)
(define/class feast (food) 'recovery-amount 150)

(define/class medicine (item) 'recovery-amount #f)
(define/class stimulant-hypo (medicine) 'recovery-amount 15)
(define/class med-kit (medicine) 'recovery-amount 45)
(define/class nano-machine-dose (medicine) 'recovery-amount 'all)


(define/class counted-list (object) 'data '() 'n 0)
(define/class limited-counted-list (counted-list) 'max-n 0)

(define-multimethod (ccons item cl) :: (class-name cl))
(define-method (ccons item cl) :: counted-list
  (adjust cl 'data
          (depending-on (data) 
                        (cons item data))
          'n
          (depending-on (n)
                        (+ n 1))))

(define-method (ccons item cl) :: limited-counted-list
  (let ((n (dict-ref cl 'n))
        (max (dict-ref cl 'max-n)))
    (if (> (+ n 1) max) #f
        (call-next-method))))

(define-multimethod (ccar cl) :: (class-name cl))
(define-method (ccar l) :: list
  (car list))
(define-method (ccar cl) :: counter-list
  (car (dict-ref cl 'data)))

(define/class kitable (object) 'shield #f 'melee-weapon #f 'ammunition #f 'items #f)
(define/class metabolizing (object) 'hunger (adjust limited-integer 'value 100 'max 100))
(define/class alive (object) 'health limited-integer)

(define-multimethod (health-as-percentage o) :: (class-name o))
(define-method (health-as-percentage o) :: alive
  (with-slots (at o 'health) (max value)
              (/ value max)))

(define-multimethod (dead? o) :: (class-name o))
(define-method (dead? o) :: alive
  (<= (dict-ref (dict-ref o 'health) 'value) 0))
(define-multimethod (initialize-health o amt) :: (class-name o))
(define-method (initialize-health o amt) :: alive
  (adjust o 'health
          (depending-on 
           (health)
           (adjust health 'max amt 'value amt))))
  
(define/class has-strength (object) 'strength limited-integer)
(define/class has-defense (object) 'defense limited-integer)
(define/class leveling (object) 'level 1)
(define/class entity (object))

(define/class player (entity kitable metabolizing alive has-strength has-defense leveling)
  'strength (adjust limited-integer 'value 3 'max 3)
  'melee-weapon bare-hands
  'shield forearm
  'health (adjust limited-integer 'value 10 'max 10))

(define/class monster (entity alive has-strength has-defense leveling))
(define/class service-droid (monster) 
  'health (adjust limited-integer 'value 4 'max 4)
  'strength 3
  'defense 1)

(define/class uncanny-duplicate (cybernetic monster)
  'health (adjust limited-integer 'value 6 'max 6)
  'strength 6
  'defense 1)

(define/class menacing-presence (supernatural monster)
  'health (adjust limited-integer 'value 8 'max 8)
  'strength 7
  'defense 3)

(define/class naked-singularity (grasping monster)
  'health (adjust limited-integer 'value 15 'max 15)
  'strength 5
  'defense 5)

(define/class industrial-robot (monster)
  'health (set-value/max limited-integer 25)
  'strength 10
  'defense 6)

(define/class combat-robot (monster)
  'health (set-value/max limited-integer 40)
  'strength 15
  'defense 7)

(define/class incisive-philosopher (monster)
  'health (set-value/max limited-integer 20)
  'strength 5
  'defense 3)


(provide (all-defined-out))