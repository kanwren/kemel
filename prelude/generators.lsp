;;; TODO: make appropriate adjustments once operand trees are implemented for
;;; continuations

($provide! ($generator run-generator next)
  ($define! arg-or-inert
    ($lambda (args)
      ($if (null? args)
        #inert
        ($sequence
          ($the null (cdr args))
          (car args)))))

  ;; Expects a lambda that takes a single parameter; a `yield` function that can
  ;; be called by the generator with a value to produce.
  ($define! run-generator
    ;; Modified from https://stackoverflow.com/a/25502400
    ($lambda (f . args)
      ($the applicative f)

      ($define! local (get-current-environment))
      ($define! prompt-cont #inert)
      ($define! iter-cont #inert)
      ($define! done #f)
      ($define! yield
        ($lambda args
          ($let/cc k
            ($set! local iter-cont k)
            (apply-continuation prompt-cont (list (arg-or-inert args))))))

      ($lambda response ; what to send back to the generator to substitute for the yield
        ($if done
          :done
          ($let/cc k
            ($set! local prompt-cont k)
            ($if (continuation? iter-cont)
              (apply-continuation iter-cont (list (arg-or-inert response)))
              ($sequence
                (apply f (cons yield args))
                ($set! local done #t)
                (apply-continuation prompt-cont (list :done)))))))))

  ($define! next
    ($lambda (f)
      ($the applicative f)
      (f)))

  ;; Above expects a labmda; provide syntax sugar to rewrite `($generator ...)`
  ;; blocks into `($lambda (yield) ...)`
  ($define! $generator
    ($macro body
      (list run-generator
            (list* $lambda
                   (list ($quote yield))
                   body)))))
