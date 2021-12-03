($provide! (solve-part-a solve-part-b)
  ($define! compare-pairs
    ($lambda (xs)
      (sum (map
             ($lambda (ys) ($if (< (car ys) (cadr ys)) 1 0))
             (list-neighbors xs)))))

  ($define! solve-part-a
    ($lambda (xs)
      (compare-pairs xs)))

  ($define! sum-triples
    ($lambda (xs) (map sum (windows 3 xs))))

  ($define! solve-part-b
    ($lambda (xs)
      (compare-pairs (sum-triples xs)))))
