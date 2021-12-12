;;;; Call-by-value lambda calculus interpreter

;; Example of factorial using the regular Y combinator in Lisp
($define! fac
  ($let
    ((y ($lambda (f) (($lambda (x) ($lambda (i) ((f (x x)) i)))
                      ($lambda (x) ($lambda (i) ((f (x x)) i)))))))
    (y ($lambda (!) ($lambda (n) ($if (= 0 n) 1 (* n (! (- n 1)))))))))

($define! eval:
  ($lambda (expr env)
    ($cond
      ((symbol? expr)
       (env expr))
      (($and? (pair? expr) (equal? ($quote lambda) (car expr)))
       ($lambda (arg)
            (eval: (caddr expr)
                   ($lambda (y) ($if (equal? y (car (cadr expr))) arg (env y))))))
      (($and? (pair? expr) (equal? ($quote if) (car expr)))
       ($if (eval: (cadr expr) env) (eval: (caddr expr) env) (eval: (cadddr expr) env)))
      ((pair? expr) ((eval: (car expr) env) (eval: (cadr expr) env)))
      (otherwise expr))))

($define! $mk-env
  ($vau bindings env
    (foldr ($lambda (binding rest)
             ($lambda (x)
               ($if (equal? x (car binding))
                 (eval (cadr binding) env)
                 (rest x))))
           ($lambda (x) ($quote variable-not-found))
           bindings)))

($define! builtins
  ($mk-env
    (zero? ($lambda (n) (= n 0)))
    (sub1 ($lambda (n) (- n 1)))
    (* ($lambda (m) ($lambda (n) (* m n))))))

;; Factorial using Y combinator in the embedded CBV lambda calculus
($define! fac:
  ($let
    ((y: ($quote
           (lambda (f) ((lambda (x) (lambda (i) ((f (x x)) i)))
                        (lambda (x) (lambda (i) ((f (x x)) i))))))))
    (list y: ($quote (lambda (!) (lambda (n) (if (zero? n) 1 ((* n) (! (sub1 n))))))))))

(print (eval: (list fac: 5) builtins))
