(print "while:")

($define! local (get-current-environment))
($define! a 0)
($while (<= a 10)
        ($set! local a (+ a 1))
        ($when (= 0 (mod a 2)) (continue))
        ($when (> a 7) (break))
        (print a))

(print "while-then:")

($define! local (get-current-environment))
($define! a 0)
($while-then (<= a 10) ($set! local a (+ a 1))
             ($when (= 0 (mod a 2)) (continue))
             ($when (> a 7) (break))
             (print a))
