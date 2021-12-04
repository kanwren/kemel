;;;; Example of cooperative threading

($define! t1-proc
  ($lambda ()
    (print "Hello from 1")
    (thread-yield)
    (print "Hello from 1 again")))
($define! t2-proc
  ($lambda ()
    (print "Hello from 2")
    (thread-yield)
    (print "Hello from 2 again")
    (thread-yield)
    (print "Hello from 2 again again")))
($define! t3-proc
  ($lambda ()
    (print "Hello from 3")
    (thread-yield)
    (print "Hello from 3 again")))

(thread-create t1-proc)
(thread-create t2-proc)
(thread-create t3-proc)
(thread-end)
