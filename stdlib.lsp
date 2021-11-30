($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals &rest body) env
                (wrap (eval `($vau ,formals _ ,@body) env))))
