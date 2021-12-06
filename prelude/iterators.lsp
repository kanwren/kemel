($provide! (list->iter iter->list for-each)
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
      ($define! local (get-current-environment))
      ($define! curr ())

      ($define! go
        ($lambda ()
          ($let ((x (iter)))
            ($if (inert? x)
              (reverse curr)
              ($sequence
                ($set! local curr (cons x curr))
                (go))))))
      (go)))

  ($define! for-each-list
    ($lambda (xs f)
      ($the applicative f)
      ($define! go
        ($lambda (xs)
          ($if (null? xs)
            #inert
            ($sequence
              (f (car xs))
              (go (cdr xs))))))
      (go xs)))

  ($define! for-each-iter
    ($lambda (iter f)
      ($the applicative f)
      ($define! go
        ($lambda ()
          ($let ((x (iter)))
            ($unless (inert? x)
              (f x)
              (go)))))
      (go)))

  ($define! for-each
    ($lambda (xs f)
      ($the applicative f)
      ($the (or list applicative) xs)
      ($if (list? xs)
        (for-each-list xs f)
        (for-each-iter xs f)))))
