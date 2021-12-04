($define! apply-continuation
  ($lambda (c o)
    (apply (continuation->applicative c) o)))

($define! $let/cc
  ($macro (k . body)
    (list call/cc (list* $lambda (list k) body))))

($define! exit
  ;; TODO: this should be #inert once operand trees are implemented
  ($lambda () (apply-continuation root-continuation (list ()))))
