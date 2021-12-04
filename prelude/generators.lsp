;;; TODO: make appropriate adjustments once operand trees are implemented for
;;; continuations

($provide! ($generator run-generator)
  ;; Expects a lambda that takes a single parameter; a `yield` function that can
  ;; be called by the generator with a value to produce.
  ($define! run-generator
    ($lambda (f)
      ($define! env (get-current-environment))

      ($define! go
        ($lambda #ignore
          ($let/cc p
            ($let
              ((yield-value
                 ($lambda (v)
                   ($let/cc k
                     ($set! env go k)
                     (apply-continuation
                       p
                       (list v))))))
              (f yield-value)))))

      ($define! gen
        ($lambda ()
          ($if (continuation? go)
            (apply-continuation go (list nil))
            (go))))

      gen))

  ;; Above expects a labmda; provide syntax sugar to rewrite `($generator ...)`
  ;; blocks into `($lambda (yield) ...)`
  ($define! $generator
    ($macro body
      (list run-generator
            (list* $lambda
                   (list ($quote yield))
                   body)))))
