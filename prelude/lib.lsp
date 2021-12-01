($define! nil ())

; TODO: decide whether or not to use this for sequence, since providing it as a
; primitive is probably significantly more efficient
($define! $_sequence
          ((wrap
             ($vau (do-both) #ignore
                   (do-both
                     ; recursive helper - sequence nonempty list of actions
                     ($define! go
                               ($vau (first . rest) env
                                     ($if (null? rest)
                                          (eval first env)
                                          (do-both (eval first env)
                                                   (eval (cons go rest) env)))))
                     ; return a vau that checks the base case and recurses
                     ($vau body env
                           ($if (null? body)
                                #inert
                                (eval (cons go body) env))))))
           ; `((wrap ($vau #ignore #ignore *b*)) *a*)` can is used to sequence
           ; two actions `*a*` and `*b*`
           ($vau (a b) env
                 ((wrap ($vau #ignore #ignore (eval b env))) (eval a env)))))

($define! $quote ($vau (x) #ignore x))

($define! car  (wrap ($vau ((x       . #ignore      )) #ignore x )))
($define! cdr  (wrap ($vau ((#ignore . xs           )) #ignore xs)))
($define! cadr (wrap ($vau ((#ignore . (x . #ignore))) #ignore x )))

($define! list (wrap ($vau xs #ignore xs))) ; $lambda xs xs
($define! list*
          (wrap
            ($vau args #ignore
                  ($sequence
                    ($define! go
                              (wrap
                                ($vau ((x . xs)) #ignore
                                      ($if (null? xs)
                                           x
                                           (cons x (go xs))))))
                    (go args)))))

($define! append
          (wrap ($vau (xs ys) #ignore
                      ($if (null? xs)
                        ys
                        (cons (car xs) (append (cdr xs) ys))))))

; Redefine $vau to be able to take an arbitrary body, not just a single
; expression
($define! $vau
          ((wrap
             ($vau ($vau) #ignore
                   ($vau (formals env-name . body) env
                         (eval
                           `(,$vau ,formals ,env-name
                                   ,($if (> (length body) 1)
                                         (cons $sequence body)
                                         (car body)))
                           env))))
          $vau))

($define! get-current-environment
          (wrap ($vau () e e)))

($define! $lambda
          ($vau (formals . body) env
                (wrap (eval `(,$vau ,formals #ignore ,@body) env))))

($define! $macro
          ($vau (formals . body) env
                ($let ((env-var (gensym)))
                  (eval `(,$vau ,formals ,env-var (,eval (,$sequence ,@body) ,env-var)) env))))

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
                  `(,$define! ,symbols
                              (,$let ()
                                     (,$sequence ,@body)
                                     (,list ,@symbols)))))

($define! $cond
          ($vau conds env
                ($if (null? conds)
                     #inert
                     ($let ((((c . body) . rest) conds))
                           ($if (eval c env)
                                (eval `(,$sequence ,@body) env)
                                (apply (wrap $cond) rest env))))))
