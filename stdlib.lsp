; TODO:
; apply
; and
; or
; not
; cond
; $set!
; $get

($define! nil ())

($define! quote ($vau (x) #ignore x))
($define! list (wrap ($vau (&rest xs) #ignore xs))) ; $lambda (&rest xs) xs
($define! list* nil) ; TODO

; TODO: move progn out of builtins
; TODO: move append out of builtins; currently needed to bootstrap the splicing syntax

($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals &rest body) env
                (wrap (eval `($vau ,formals #ignore ,@body) env))))

($define! $macro
          ($vau (formals &rest body) env
                (let ((env-var (gensym)))
                  (eval `($vau ,formals ,env-var (eval (progn ,@body) ,env-var)) env))))

($define! car ($lambda (head &rest tail) head))
($define! cdr ($lambda (head &rest tail) tail))
($define! cadr ($lambda (xs) (car (cdr xs))))

($define! map
          ($lambda (f xs)
                   (if (null xs)
                     ()
                     (cons (f (car xs)) (map f (cdr xs))))))

($define! let
          ($vau (bindings &rest body) env
                (eval
                  `(($lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
                  env)))

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
