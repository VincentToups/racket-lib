(doublet-a (car (doublet-a ((mlet* m-turtles
        ((_ (setl 'x 100))
         (x (getl 'x)))
        (m-return x)) init-state))))

(doublet-b (car (doublet-a ((setl 'x 100) init-state))))

(cons (cons 'x 'y) '())

(list (pair 'x 'y))

(dict-set '() 'x 10)

(cdr (pair 'x (dict-set '() 'x 10)))

(match (pair 'x (dict-set '() 'x 10))
  [(cons cr dr)
   (vector cr dr)])