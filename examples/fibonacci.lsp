;;;; Example of using generators

($define! fibs
  ($lambda ()
    ($generator
      ($define! go
        ($lambda (n0 n1)
          (yield n0)
          (go n1 (+ n0 n1))))
      (go 0 1))))

($define! fibs-before-100
  ($lambda ()
    ($generator
      ($define! go
        ($lambda (n0 n1)
          ($when (< n0 100)
            (yield n0)
            (go n1 (+ n0 n1)))))
      (go 0 1))))
