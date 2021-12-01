; TODO:
; and
; or
; not
; when
; unless
; cond

($define! nil ())

($define! $quote ($vau (x) #ignore x))
($define! list (wrap ($vau (&rest xs) #ignore xs))) ; $lambda (&rest xs) xs
($define! list* nil) ; TODO

; TODO: move $sequence out of builtins
; TODO: move append out of builtins; currently needed to bootstrap the splicing syntax

($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals &rest body) env
                (wrap (eval `(,$vau ,formals #ignore ,@body) env))))

($define! $macro
          ($vau (formals &rest body) env
                ($let ((env-var (gensym)))
                  (eval `(,$vau ,formals ,env-var (,eval (,$sequence ,@body) ,env-var)) env))))

($define! cadr ($lambda (xs) (car (cdr xs))))

($define! apply
          ($lambda (op args &optional env)
                   (eval
                     `(,(unwrap op) ,@args)
                     ($if (null? env) (make-environment) env))))

($define! map
          ($lambda (f xs)
                   ($if (null? xs)
                        ()
                        (cons (f (car xs)) (map f (cdr xs))))))

($define! $let
          ($vau (bindings &rest body) env
                (eval
                  `((,$lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
                  env)))

($define! $set!
          ($vau (target-env binders vals) env
                (eval
                  `(,$define! ,binders (,(unwrap eval) ,vals ,env))
                  (eval target-env env))))

($define! $get
          ($vau (target-env binder) env
                (eval binder (eval target-env env))))

($define! $provide!
          ($macro (symbols &rest body)
                  (the list symbols) ; can't be single bare variable
                  `(,$define! ,symbols (,$let () (,$sequence ,@body) (,list ,@symbols)))))
