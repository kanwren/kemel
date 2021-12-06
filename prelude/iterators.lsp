($provide! (list->iter iter->list for/iter map/iter filter/iter foldr/iter foldl/iter append/iter take/iter drop/iter zip-with/iter range)
  ($define! list->iter
    ($lambda (xs)
      ($generator
        ($define! go
          ($lambda (xs)
            ($unless (null? xs)
              (yield (car xs))
              (go (cdr xs)))))
        (go xs))))

  ($define! iter->list
    ($lambda (iter)
      ($define! go
        ($lambda (acc)
          ($let ((x (next iter)))
            ($if (equal? x :done)
              (reverse acc)
              (go (cons x acc))))))
      (go ())))

  ($define! take/iter
    ($lambda (n iter)
      ($generator
        ($define! go
          ($lambda (i)
            ($when (< i n)
              (yield (next iter))
              (go (+ i 1)))))
        (go 0))))

  ($define! drop/iter
    ($lambda (n iter)
      ($generator
        ($define! go
          ($lambda (i)
            ($if (< i n)
              ($sequence (next iter) (go (+ i 1)))
              ($sequence (yield (next iter)) (rest)))))
        ($define! rest
          ($lambda () (yield (next iter)) (rest)))
        (go 0))))

  ($define! map/iter
    ($lambda (f iter)
      ($the applicative f)
      ($generator
        ($define! go
          ($lambda ()
            ($let ((x (next iter)))
              ($if (equal? x :done)
                ($sequence (yield x) (go))
                ($sequence (yield (f x)) (go))))))
        (go))))

  ($define! for/iter
    ($lambda (iter f)
      ($the applicative f)
      ($define! go
        ($lambda ()
          ($let ((x (next iter)))
            ($unless (equal? x :done)
              (f x)
              (go)))))
      (go)))

  ($define! filter/iter
    ($lambda (f iter)
      ($the applicative f)
      ($generator
        ($define! go
          ($lambda ()
            ($let ((x (next iter)))
              ($if (equal? x :done)
                ($sequence (yield x) (go))
                ($if (f x)
                  ($sequence (yield x) (go))
                  (go))))))
        (go))))

  ($define! foldr/iter
    ($lambda (f z iter)
      ($the applicative f)
      ($define! go
        ($lambda ()
          ($let ((x (next iter)))
            ($if (equal? x :done)
              z
              (f x (go))))))
      (go)))

  ($define! foldl/iter
    ($lambda (f z iter)
      ($the applicative f)
      ($define! go
        ($lambda (acc)
          ($let ((x (next iter)))
            ($if (equal? x :done)
              acc
              (go (f acc x))))))
      (go z)))

  ($define! append/iter
    ($lambda (iter1 iter2)
      ($generator
        ($define! go
          ($lambda ()
            ($let ((x (next iter1)))
              ($if (equal? x :done)
                (rest)
                ($sequence (yield x) (go))))))
        ($define! rest
          ($lambda ()
            (yield (next iter2))
            (rest)))
        (go))))

  ($define! zip-with/iter
    ($lambda (f iter1 iter2)
      ($the applicative f)
      ($generator
        ($define! go
          ($lambda ()
            ($let ((x (next iter1)) (y (next iter2)))
              ($sequence (yield ($if ($or? (equal? x :done) (equal? y :done)) x (f x y))) (go)))))
        (go))))

  ;; Example calls:
  ;; (range 20) -> [0, 20)
  ;; (range 10 20) -> [10, 20)
  ;; (range 10 #ignore) -> [10, infinity)
  ;; (range 10 100 5) -> [10, 15, 20, ..., 100)
  ;; (range 10 #ignore 5) -> [10, 15, 20, ...)
  ($define! range
    ($lambda (first . rest)
      ($cond
        ((null? rest) ; upper bound
         (range 0 first 1))
        ((null? (cdr rest)) ; lower and upper bound
         (range first (car rest) 1))
        ((null? (cddr rest))
         ($let ((upper (car rest)) (step (cadr rest)))
           ($generator
             ($define! go
               ($lambda (n)
                 ($when ($or? (ignore? upper) (< n upper))
                   (yield n)
                   (go (+ n step)))))
             (go first))))
        (otherwise ($the null (cdddr rest)))))))
