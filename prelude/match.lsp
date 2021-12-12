;;;; Pattern matching

;; TODO: this implementation is currently horrendously slow

;; TODO: handle more patterns:
;; - ($quote s-expr): equal s expression
;; - ($ record-name fields): record type satisfying record-name?, unpacking into fields

($provide! ($match $lambda-match)
 ($define! match-pat
   ($lambda (pattern value env)
     ($cond
       ((ignore? pattern) #t)
       ((symbol? pattern)
        (eval (list $set! env pattern ($quote value)) (get-current-environment))
        #t)
       (($or?
          (null? pattern)
          (inert? pattern)
          (bool? pattern)
          (keyword? pattern)
          (number? pattern)
          (string? pattern))
        (equal? pattern value))
       ((pair? pattern)
        ($let ((head (car pattern)))
          ($cond
            ((equal? head ($quote when))
             ($the null (cddr pattern)) ; enforce when only has one argument
             (eval (cadr pattern) env))
            ((equal? head ($quote and))
             (all? ($lambda (subpat) (match-pat subpat value env)) (cdr pattern)))
            ((equal? head ($quote or))
             (or? ($lambda (subpat) (match-pat subpat value env)) (cdr pattern)))
            ;; ((equal? head ($quote $)) "TODO")
            ((pair? value)
             (match-pat (car pattern) (car value) env)
             (match-pat (cdr pattern) (cdr value) env))
            (#t #f))))
       (#t #f))))

 ($define! match-pats
   ($lambda (env val pats)
     ($define! go
       ($lambda (patterns)
         ($if (null? patterns)
           #inert
           ($let ((((pat . body) . rest-pats) patterns)
                  (sub-env (make-environment env))) ; TODO: only allocate environment if necessary (pattern contains when guard)
             ($if (match-pat pat val sub-env)
               (eval (cons $sequence body) sub-env)
               (go rest-pats))))))
     (go pats)))

 ($define! $match
   ($vau (expr . pats) env
     (match-pats env (eval expr env) pats)))

 ($define! $lambda-match
   ($vau pats env
     ($lambda args (match-pats env args pats)))))
