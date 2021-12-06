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

(print "for-each:")
($for-each x (range 1 11)
  ($when (= 0 (mod x 2)) (continue))
  ($when (> x 7) (break))
  (print x))

