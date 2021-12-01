($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals &rest body) env
                (wrap (eval `($vau ,formals _ ,@body) env))))

($define! $macro
          ($vau (formals &rest body) env
                (let ((env-var (gensym)))
                  (eval `($vau ,formals ,env-var (eval (progn ,@body) ,env-var)) env))))

($define! cond
          ($vau (&rest conds) env
                (eval
                  (foldr
                    (lambda (clause1 rest)
                      `(if
                         ,(car clause1)
                         (progn ,@(cdr clause1))
                         ,rest))
                    nil conds)
                  env)))
