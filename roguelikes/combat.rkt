#lang racket

(require utilities/proletariat
         utilities/simple-infix
         (planet "main.ss" ("murphy" "multimethod.plt" 2 1))
         (rename-in (only-in racket/base floor) [floor n.floor])
         functional/point-free
         roguelikes/white-whale-data3)

(define/class attack-value (object) 'magnitude #f 'modifiers '())

(define (randomize n p)
  (let ((amt 
         (match (inexact->exact (ceiling (* n p)))
           [0 2]
           [(? even? n) n]
           [(? odd? n) (+ n 1)])))
    (+ n (- (random amt) (/ amt 2)))))
        

(define randomize10 (partial< randomize 0.1))
(define randomize20 (partial< randomize 0.2))
(define randomize30 (partial< randomize 0.3))

(define-multimethod (attack-power obj) :: (class-name obj))
(define-multimethod (defense-power obj) :: (class-name obj))

(define-syntax attack-power-of
  (syntax-rules ()
    [(attack-power-of symbol body ...)
     (define-method (attack-power symbol) :: symbol 
       body ...)]))

(define-syntax defense-power-of 
  (syntax-rules ()
    [(defense-power-of symbol body ...)
     (define-method (defense-power symbol) :: symbol 
       body ...)]))

(attack-power-of 
 melee-weapon 
 (+ (as (dict-ref melee-weapon 'attack) 'number)
    (as (dict-ref melee-weapon 'modifier) 'number)))

(defense-power-of 
  shield
  (+ (as (dict-ref shield 'defense) 'number)
     (as (dict-ref shield 'modifier) 'number)))

(define-method (attack-power thing) :: boolean
  0)

(define-method (defense-power thing) :: boolean 
  0)

(attack-power-of monster (dict-ref monster 'strength))
(attack-power-of player  (+ (as (dict-ref player 'strength) 'number)
                            (as (attack-power (dict-ref player 'melee-weapon)) 'number)))

(defense-power-of monster (as (dict-ref monster 'defense) 'number))
(defense-power-of player (+ (as (dict-ref player 'defense) 'number)
                            (as (defense-power (dict-ref player 'shield)) 'number)))

(define-multimethod (calc-melee-attack agressor target) :: (vector-immutable (class-name agressor)
                                                                        (class-name target)))

(define (intersection l1 l2)
  (filter (partial< member l2) l1))

(struct combat-result (actor actee side-effects) #:transparent)

(define-syntax when/empty 
  (syntax-rules ()
    [(when-or-empty pred body ...)
     (if pred (begin body ...) (list))]))

(define-method (calc-melee-attack ag ta) :: #(player monster)
  (let* ((atk (attack-power ag))
         (def (defense-power ta))
         (m-traits (monster-traits ta))
         (w-traits (melee-weapon-traits (dict-ref ag 'melee-weapon)))
         (self-damage (if (self-damaging? ($ ag at 'melee-weapon)) 
                          (min 1 (* ($ ag -> 'melee-weapon 'reciprocal-damage-percentage) (randomize30 atk)))
                          0))
         (weapon-breaks? 
          (if (breakable? ($ ag at 'melee-weapon))
              (< (random) ($ ag -> 'melee-weapon 'break-probability))
              #f))
         (bonus-count (length (intersection m-traits w-traits)))
         (modified-atk  (+ atk (* 0.1 atk bonus-count)))
         (hp-delta (max 1 (- (randomize10 modified-atk) (randomize10 def))))
         (new-weapon
          (track-use (if weapon-breaks? bare-hands ($ ag at 'melee-weapon)))))
    (combat-result 
     (adjust ag 'melee-weapon
             new-weapon
             'health 
             (depending-on (health)
                           (lim- health self-damage)))
     (adjust ta 'health (depending-on (health) (lim- health hp-delta)))
     (append
      (when/empty weapon-breaks? '(weapon-breaks))
      (when/empty (> self-damage 0) (list (list 'self-damage self-damage)))))))

(define-method (calc-melee-attack ag ta) :: #(monster player)
  (let* ((atk (attack-power ag))
         (def (defense-power ta))
         (m-traits (monster-traits ag))
         (s-traits (shield-traits (dict-ref ta 'shield)))
         (bonus-count (length (intersection m-traits s-traits)))
         (modified-defense (+ def (* 0.1 def bonus-count)))
         (hp-delta (max 1 (- (randomize10 atk) (randomize10 modified-defense)))))
    (combat-result
     ag
     (adjust ta 'health 
             (depending-on 
              (health)
              (lim- health hp-delta))
             'shield (depending-on 
                      (shield)
                      (track-use shield)))
     '())))
             








