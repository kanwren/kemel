;;;; A basic logic programming framework

;;; TODO: this needs a lot more combinators to be usable

($provide! (lvar lvar? walk unify conj disj pure mzero ==)

  ;; quick and dirty implementation of maps based on key-value association lists
  ;; TODO: rewrite against trees or hashmaps
  ($define! lookup
    ($lambda (needle alist)
      ($let
        ((res (assoc needle alist)))
        ($if (null? res)
          ()
          (list (cdr res))))))
  ($define! insert
    ($lambda (k v alist)
      ($if (null? alist)
        (list (cons k v))
        ($let
          (((h . t) alist))
          ($if (equal? (car h) k)
            (cons (cons k v) t)
            (cons h (insert k v t)))))))

  ($define! lvar
    ($lambda (x)
      (string->symbol (string-append x "_" (symbol->string (gensym))))))
  ($define! lvar? symbol?)

  ($define! walk
    ($lambda (m u)
      ($let ((pr (lookup u m)))
        ($if (null? pr)
          u
          ($if (lvar? (car pr)) (walk m (car pr)) (car pr))))))

  ($define! unify
    ($lambda (m u v)
      ($let
        ((u (walk m u))
         (v (walk m v)))
        ($cond
          (($and? (lvar? u)
                  (lvar? v)
                  (equal? u v))
           m)
          ((lvar? u) (insert u v m))
          ((lvar? v) (insert v u m))
          (#t ($if (equal? u v) m #f))))))

  ($define! pure ($lambda (m) (list m)))
  ($define! mzero ($lambda (m) ()))

  ($define! conj
    ($lambda goals
      (foldr ($lambda (a b)
               ($lambda (m) (concat-map b (a m))))
             pure
             goals)))

  ($define! disj
    ($lambda goals
      ($lambda (m)
        (concat-map ($lambda (goal) (goal m)) goals))))

  ($define! ==
    ($lambda (a b)
      ($lambda (m)
        ($let ((v (unify m a b)))
          ($if (equal? v #f) () (list v)))))))
