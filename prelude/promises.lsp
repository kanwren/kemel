;;;; Promises - lazy computations whose results can later be forced and memoized

($provide! (promise? memoize $lazy force)

  ;; A promise is an encapsulation containing a mutable box containing some
  ;; context, a pair of an object and nil if evaluated or an expression and an
  ;; environment if not.
  ($define! (promise-caps promise? promise-decaps) (make-encapsulation-type))
  ($declare-record-type! promise-ctx (obj env))

  ($define! memoize
    ($lambda (value)
      (promise-caps (box (promise-ctx value ())))))

  ($define! $lazy
    ($vau (exp) env
      (promise-caps (box (promise-ctx exp env)))))

  ($define! force
    ($lambda (x)
      ($if (promise? x)
        (force-promise (promise-decaps x))
        x)))

  ($define! force-promise
    ($lambda (b) ; box containing promise-ctx
      ($let* ((ctx (unbox b))
              (obj (promise-ctx-obj ctx))
              (env (promise-ctx-env ctx)))
        ($if (not? (environment? env))
          obj
          (handle-promise-result b ctx obj env (eval obj env))))))

  ($define! handle-promise-result
    ($lambda (b ctx obj env val) ; box containing promise-ctx, promise-ctx, unpacked promise-ctx, and evaluated value
      ($cond
        ((null? env) obj)
        ((not? (promise? val))
         ;; memoize and return
         (set-promise-ctx-obj! ctx val)
         (set-promise-ctx-env! ctx ())
         val)
        (otherwise
          ;; val is a promise; repeat
          (set-box! b (unbox (promise-decaps val)))
          (force-promise b))))))
