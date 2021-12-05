;;;; Example of cooperative threading

($thread
  (print "Hello from 1")
  (thread-yield)
  (print "Hello from 1 again"))

($thread
  (print "Hello from 2")
  (thread-yield)
  (print "Hello from 2 again")
  (thread-yield)
  (print "Hello from 2 again again"))

($thread
  (print "Hello from 3")
  (thread-yield)
  (print "Hello from 3 again"))

;; Start the thread pool and wait for all to terminate
(thread-join)
