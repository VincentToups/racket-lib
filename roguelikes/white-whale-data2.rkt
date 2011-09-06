#lang racket

(require racket/dict
         utilities/struct-with-dippers)

(struct/dippers thing (properties) #:transparent)
(define default-thing (thing '()))

(define (set-property th property value)
  (dip-thing-properties 
   th 
   (lambda (properties)
     (dict-set properties property value))))

(define (get-property th property . args)
  (match args
    [(list) (get-property th property (lambda () (error "No property of thing ~a called ~a." th property)))]
    [(list fail)
        (match th
          [(struct* thing ([properties p]))
           (dict-ref p property fail)])]))

(define (set-properties th . rest)
  (match rest
    [(list) th]
    [(cons key (cons val rest))
     (apply set-properties (set-property th key val)
            rest)]
    [(list stub)
     (error "Set properties requires an even number of additional arguments.")]))

(define (unset-property th key)
  (dip-thing-properties th key
                        (lambda (properties)
                          (dict-remove properties key))))

(struct/dippers tile thing (things entities) #:transparent)
(define default-tile (tile '() '() '()))

(define (tile-add-thing tile thing)
  (dip-tile-things tile 
                   (lambda (things)
                     (cons thing things))))

(define (tile-filter-things tile pred)
  (dip-tile-things tile
                   (lambda (things)
                     (filter pred things))))

(define (tile-remove-thing tile thing)
  (tile-filter-things tile
                      (lambda (thing*) (not (equal? thing thing*)))))

(define (tile-count-things tile)
  (length (tile-things tile)))

(define (tile-add-entity tile entity)
  (dip-tile-entities tile 
                   (lambda (entities)
                     (cons entity entities))))

(define (tile-filter-entities tile pred)
  (dip-tile-entities tile
                   (lambda (entities)
                     (filter pred entities))))

(define (tile-remove-entity tile entity)
  (tile-filter-entities tile
                      (lambda (entity*) (not (equal? entity entity*)))))

(define (tile-count-entities tile)
  (length (tile-entities tile)))

(struct/dippers wall tile () #:transparent)
(struct/dippers floor tile () #:transparent)
(struct/dippers fluid (depth) #:transparent)
(struct/dippers exit () #:transparent)
(struct/dippers trigger (do) #:transparent)

(struct/dippers water fluid () #:transparent)
(struct/dippers lava  fluid  () #:transparent)
(struct/dippers upward-staircase (destination) #:transparent)
(struct/dippers downward-staircase (destination) #:transparent)
(struct/dippers lateral-exit (direction) #:transparent)

(struct/dippers definite-ratio (n d) #:transparent)
(define (num+ r a)
  (match r
    [(definite-ratio n d) 
     (let ((n (+ n a)))
       (cond ((> n d) (definite-ratio d d))
             ((< n 0) (definite-ratio 0 d))
             (else (definite-ratio n d))))]))
(define (num- r a) (num+ r (- a)))
(define (den+ r a)
  (dip-definite-ratio-d r
                        (lambda (d)
                          (+ d a))))
(define (den- r a)
  (den+ r (- a)))
           

(struct/dippers entity thing (health attack defense speed brain facing) #:transparent)
(struct/dippers player entity (items shield weapon melee-weapon hunger) #:transparent)
(struct/dippers mysterious-robot entity () #:transparent)
(struct/dippers awkward-copy entity () #:transparent)
(struct/dippers menacing-presence () #:transparent)
(struct/dippers naked-singularity () #:transparent)
(struct/dippers industrial-robot  () #:transparent)
(struct/dippers combat-robot () #:transparent)
(struct/dippers incisive-philosopher () #:transparent)

(struct/dippers item thing (curse-status) #:transparent)

(define (curse item)
  (set-item-curse-status item 'cursed))

(define (uncurse item)
  (set-item-curse-status item 'uncursed))

(struct/dippers modifiable-item item (modifier) #:transparent)

(struct/dippers shield modifiable-item (defense) #:transparent)
(struct/dippers melee-weapon modifiable-item (attack) #:transparent)
(struct/dippers food item (recovery-amount) #:transparent)
(struct/dippers medicine item (recovery-amount) #:transparent)
(struct/dippers meta-program item () #:transparent)

(struct/dippers loose-paneling shield () #:transparent)
(struct/dippers bulkhead-fragment shield () #:transparent)
(struct/dippers energy-shield shield (charge-decrement charge) #:transparent)
(struct/dippers medical-system () #:transparent)
(struct/dippers defensive-symbiote () #:transparent)

(define (get-intrinsic-shield-defense s)
  (shield-defense s))

(define (get-shield-defense s)
  (+ (get-intrinsic-shield-defense s) (modifiable-item-modifier s)))

(define (apply-modifier mi m)
  (dip-modifiable-item-modifier 
   (lambda (mi)
     (num+ mi m))))

(define (get-intrinsic-shield-properties s)
  (match s
    [(? loose-paneling?) '()]
    [(? bulkhead-fragment?) '()]
    [(struct* energy-shield ([charge-decrement cd]
                             [charge c]))
              (list (cons 'active-shielding cd)
                    (cons 'charge c))]
    [(? medical-system?) '((hunger-regulating . #t))]
    [(? defensive-symbiote?) '((appetite-producing . #t))]))

(define (get-shield-properties s)
  (let* ([intrinsics (get-intrinsic-shield-properties s)]
         [extrinsics (get-property s 'extra-shield-properties '())])
    (foldl 
     (lambda (pair properties)
       (match pair
         [(cons key val)
          (dict-set properties key val)]))
     intrinsics
     extrinsics)))

(define (shield-hunger-multiplier s)
  (let ((props (get-shield-properties s)))
    (match (list (dict-ref props 'hunger-regulating #f)
                 (dict-ref props 'appetite-producing #f))
      [(list #f #f) 1.0]
      [(list #t #f) 0.5]
    [(list #t #t) 1.0]
      [(list #f #t) 2.0])))
  
(struct/dippers suit-gauntlet melee-weapon () #:transparent)
(struct/dippers heavy-mechanism melee-weapon () #:transparent)
(struct/dippers memory-crystal-shard () #:transparent)
(define (get-self-damage-amount w)
  (match w
    [(? memory-crystal-shard?) 3]))
    
(struct/dippers topiary-fractal () #:transparent)

(define (get-break-probability w)
  (match w
    [(? memory-crystal-shard?) .1]
    [(? topiary-fractal?) .3]))

(struct/dippers energy-sword (charge-decrement charge) #:transparent)

(define (get-charge-decrement i)
  (match i
    [(or (? energy-sword?)
         (? energy-shield?)) 7]))

(struct/dippers wire-bundle  () #:transparent)
(struct/dippers electric-prod () #:transparent)
(struct/dippers ouija-shiv    () #:transparent)
(struct/dippers occams-razor  () #:transparent)

(define (get-intrinsic-melee-weapon-attack w)

(define (get-intrinsic-melee-weapon-properties m)
  (match m
    [(or 
      (? suit-gauntlet?)
      (? heavy-mechanism?)) '()]
    [(struct* memory-crystal-shard ([break-probability b]
                                    [self-damage-amount s]))
     (list (cons 'breakable b)
           (cons 'double-edged s))]
    [(struct* topiary-fractal ([break-probability b]))
              (list (cons 'breakable b)
                    (cons 'topiary   #t))]
    [(struct* energy-sword ([charge-decrement cd]
                            [charge c]))
              (list
               (cons 'active-offense cd)
               (cons 'charge c))]
    [(? wire-bundle?)
     (list (cons 'extended-range #t))]
    [(? electric-prod?)
     (list (cons 'robot-killing #t))]
    [(? ouija-shiv?)
     (list (cons 'supernatural-killing #t))]
    [(? occams-razor?)
     '()]))

(define (get-melee-weapon-properties m)
  (let ((intrinsics (get-intrinsic-melee-weapon-properties m))
        (extrinsics (get-property m 'extra-melee-weapon-properties '())))
    (foldl 
     (lambda (key props)
       (dict-set props key (dict-ref extrinsics key)))
     intrinsics
     (dict-keys extrinsics))))
