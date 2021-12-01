($define! nil ())

($define! $quote ($vau (x) #ignore x))
($define! list (wrap ($vau xs #ignore xs))) ; $lambda xs xs
($define! list* nil) ; TODO

; TODO: move $sequence out of builtins
; TODO: move append out of builtins; currently needed to bootstrap the splicing syntax

($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals . body) env
                (wrap (eval `(,$vau ,formals #ignore ,@body) env))))

($define! $macro
          ($vau (formals . body) env
                ($let ((env-var (gensym)))
                  (eval `(,$vau ,formals ,env-var (,eval (,$sequence ,@body) ,env-var)) env))))

($define! car ($lambda ((x . #ignore))  x))
($define! cdr ($lambda ((#ignore . xs)) xs))
($define! cadr ($lambda ((#ignore . (x . #ignore))) x))

($define! not? ($lambda (x) ($if x #f #t)))

($define! $and?
          ($vau conds env
                ($if (null? conds)
                     #t
                     ($if (eval (car conds) env)
                          (apply (wrap $and?) (cdr conds) env)
                          #f))))

($define! $or?
          ($vau conds env
                ($if (null? conds)
                     #f
                     ($if (eval (car conds) env)
                          #t
                          (apply (wrap $or?) (cdr conds) env)))))

($define! $when
          ($vau (cond . body) env
                ($if (eval cond env)
                     (eval `(,$sequence ,@body) env)
                     #inert)))

($define! $unless
          ($vau (cond . body) env
                ($if (eval cond env)
                     #inert
                     (eval `(,$sequence ,@body) env))))

($define! apply
          ($lambda (op args . optional-env)
                   (eval
                     `(,(unwrap op) ,@args)
                     ($if (null? optional-env)
                          (make-environment)
                          ($sequence
                            ; Assert there's only one optional parameter
                            (the null (cdr optional-env))
                            (car optional-env))))))

($define! map
          ($lambda (f xs)
                   ($if (null? xs)
                        ()
                        (cons (f (car xs)) (map f (cdr xs))))))

($define! $let
          ($vau (bindings . body) env
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
          ($macro (symbols . body)
                  (the list symbols) ; can't be single bare variable
                  `(,$define! ,symbols (,$let () (,$sequence ,@body) (,list ,@symbols)))))

($define! $cond
          ($vau conds env
                ($if (null? conds)
                     #inert
                     ($let ((((c . body) . rest) conds))
                           ($if (eval c env)
                                (eval `(,$sequence ,@body) env)
                                (apply (wrap $cond) rest env))))))
