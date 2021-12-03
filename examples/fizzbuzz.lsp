($provide! (fizzbuzz)
  ($define! divides ($lambda (n x) (= 0 (mod x n))))

  ($define! single
    ($lambda (x)
      ($cond
        ((divides 15 x) (print "FizzBuzz"))
        ((divides 3 x)  (print "Fizz"))
        ((divides 5 x)  (print "Buzz"))
        (otherwise      (print x))
        )))

  ($define! fizzbuzz
    ($lambda (upper)
      ($define! go
        ($lambda (n)
          ($unless (> n upper)
            (single n)
            (go (+ n 1)))
          ))
      (go 1))))
