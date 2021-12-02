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

($define! car  (wrap ($vau ((x       . #ignore)) #ignore x )))
($define! cdr  (wrap ($vau ((#ignore . xs     )) #ignore xs)))
($define! cadr (wrap ($vau ((#ignore . (x       . #ignore))) #ignore x )))
($define! cddr (wrap ($vau ((#ignore . (#ignore . xs     ))) #ignore xs)))
($define! caddr (wrap ($vau ((#ignore . (#ignore . (x       . #ignore)))) #ignore x )))
($define! cdddr (wrap ($vau ((#ignore . (#ignore . (#ignore . xs     )))) #ignore xs)))

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
  (wrap
    ($vau (xs ys) #ignore
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
                   ,($if (null? body)
                      nil
                      ($if (> (length body) 1)
                        (cons $sequence body)
                        (car body))))
           env))))
   $vau))

($define! $lambda
  ($vau (formals . body) env
    (wrap (eval `(,$vau ,formals #ignore ,@body) env))))

($define! $macro
  ($vau (formals . body) env
    ; Don't have let yet, have to use the `(($lambda params ...) vals)` trick
    ((wrap ($vau (env-var) #ignore
        (eval `(,$vau ,formals ,env-var (,eval (,$sequence ,@body) ,env-var)) env)))
     (gensym))))

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
    ($define! go
      ($lambda (xs)
        ($if (null? xs)
          nil
          (cons (f (car xs)) (go (cdr xs))))))
    (go xs)))

;;;;; Environments

($define! get-current-environment
  (wrap ($vau () e e)))

($define! make-kernel-standard-environment
  ($lambda () (get-current-environment)))

; Evaluate an expression in the provided environment
($define! $remote-eval
  ($vau (exp target-env) env
    (eval exp (eval target-env env))))

; Set variables in the given environment
($define! $set!
  ($vau (target-env binders vals) env
    (eval
      `(,$define! ,binders (,(unwrap eval) ,vals ,env))
      (eval target-env env))))

; Look up a variable in the given environment
; This is just $remote-eval, but with the arguments flipped
($define! $get
  ($vau (target-env exp) env
    (eval exp (eval target-env env))))

; Run a sequence of commands, and bind the provided symbols from the resulting
; environment into the current environment
($define! $provide!
  ($macro (symbols . body)
    (the list symbols) ; can't be single bare variable
    `(,$define! ,symbols
                (,$let ()
                       (,$sequence ,@body)
                       (,list ,@symbols)))))

; Import the given symbols from an environment into the current environment
($define! $import!
  ($vau (env-exp . symbols) env
    (eval `(,$set! ,env ,symbols (,list ,@symbols))
          (eval env-exp env))))

;;;;; Lets

; Rewrites `($let (params-with-values) ...)` into `(($lambda params ...) values)`
($define! $let
  ($macro (bindings . body)
    (the list bindings)
    `((,$lambda ,(map car bindings) ,@body) ,@(map cadr bindings))))

; Rewrite `($let* (params-with-values) ...)` into `($let ((param val)) ($let ((param val)) ...))`
($define! $let*
  ($macro (bindings . body)
    (the list bindings)
    ($if (null? bindings)
      `(,$let ,bindings ,@body)
      `(,$let (,(car bindings)) (,$let* ,(cdr bindings) ,@body)))))

; Rewrite `($letrec (params-with-values) ...)` into `($let () ($define! params values) ...)`
($define! $letrec
  ($macro (bindings . body)
    (the list bindings)
    `(,$let ()
            (,$define!
              ,(map car bindings)
              (,list ,@(map cadr bindings)))
            ,@body)))

; Rewrite `($letrec* (params-with-values) ...)` into `($letrec ((param val)) ($letrec ((param val)) ...))`
($define! $letrec*
  ($macro (bindings . body)
    (the list bindings)
    ($if (null? bindings)
      `(,$letrec ,bindings ,@body)
      `(,$letrec (,(car bindings)) (,$letrec* ,(cdr bindings) ,@body)))))

($define! $let-redirect
  ($vau (env-exp bindings . body) env
    (eval `(,(eval `(,$lambda ,(map car bindings) ,@body)
                   (eval env-exp env))
             ,@(map cadr bindings))
          env)))

($define! $let-safe
  ($macro (bindings . body)
    `(,$let-redirect (,make-kernel-standard-environment) ,bindings ,@body)))

;;;;; Conditionals

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

($define! $cond
  ($vau conds env
    ($if (null? conds)
      #inert
      ($let ((((c . body) . rest) conds))
        ($if (eval c env)
          (eval `(,$sequence ,@body) env)
          (apply (wrap $cond) rest env))))))

($define! otherwise #t)

;;;;; Lists

($define! foldr
  ($lambda (f z xs)
    ($define! go
      ($lambda (xs)
        ($if (null? xs)
          z
          (f (car xs) (go (cdr xs))))))
    (go xs)))

($define! foldl
  ($lambda (f z xs)
    ($define! go
      ($lambda (acc xs)
        ($if (null? xs)
          acc
          (go (f acc (car xs)) (cdr xs)))))
    (go z xs)))

($define! take
  ($lambda (n xs)
    ($if ($or? (<= n 0) (null? xs))
      nil
      (cons (car xs) (take (- n 1) (cdr xs))))))

($define! drop
  ($lambda (n xs)
    ($if ($or? (<= n 0) (null? xs))
      xs
      (drop (- n 1) (cdr xs)))))

($define! windows
  ($lambda (n xs)
    ($define! go
      ($lambda (rem xs)
        ($if (< rem n)
          nil
          (cons (take n xs) (go (- rem 1) (cdr xs))))))
    (go (length xs) xs)))

($define! sum ($lambda (xs) (apply + xs)))
($define! product ($lambda (xs) (apply * xs)))

; TODO: bounds check
($define! index
  ($lambda (n xs)
    ($if (= n 0)
      (car xs)
      (index (- n 1) (cdr xs)))))

($define! filter
  ($lambda (pred xs)
    ($define! go
      ($lambda (xs)
        ($if (null? xs)
          nil
          ($if (pred (car xs))
            (cons (car xs) (go (cdr xs)))
            (go (cdr xs))))))
    (go xs)))

($define! zip
  ($lambda (xs ys)
    ($if ($or? (null? xs) (null? ys))
      nil
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys))))))
