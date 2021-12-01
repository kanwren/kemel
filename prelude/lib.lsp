; TODO:
; apply
; and
; or
; not
; when
; unless
; cond
; $set!
; $get

($define! nil ())

($define! $quote ($vau (x) #ignore x))
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
                  `(($lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
                  env)))
