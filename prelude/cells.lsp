;;;; Mutable cons cells

($provide! (cell? cell-cons cell-car cell-cdr set-cell-car! set-cell-cdr!)
  ($declare-record! cell (car cdr))
  ($define! cell-cons cell))

($define! list->cell
  ($lambda (xs)
    ($cond
      ((pair? xs) (cell-cons (car xs) (list->cell (cdr xs))))
      ((null? xs) nil)
      (#t xs))))

($define! cell->list
  ($lambda (xs)
    ($if (cell? xs)
      (cons (cell-car xs) (cell->list (cell-cdr xs)))
      xs)))

