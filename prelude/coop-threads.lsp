;;; TODO: make appropriate adjustments once operand trees are implemented for
;;; continuations

($provide! (thread-yield thread-create $thread thread-join)

  ;; The thread queue
  ($provide! (has-pending-threads queue-thread dequeue-thread swap-thread)
    ($define! env (get-current-environment))
    ($define! thread-list ())
    ($define! has-pending-threads ($lambda () (not? (null? thread-list))))
    ($define! queue-thread
      ($lambda (thread)
        ($set! env thread-list (append thread-list (list thread)))))
    ($define! dequeue-thread
      ($lambda ()
        ($let (((next-thread . rest-threads) thread-list))
          ($set! env thread-list rest-threads)
          next-thread)))
    ($define! swap-thread
      ($lambda (thread)
        (queue-thread thread)
        (dequeue-thread))))

  ;; Yield the currently executing thread, add it back to the queue, and switch
  ;; to the next thread in the queue
  ($define! thread-yield
    ($lambda ()
      ($when (has-pending-threads)
        ($let ((cont ($let/cc k k)))
          ($when (continuation? cont)
            (apply-continuation (swap-thread cont) (list :thread-yield)))))))

  ;; Given a function, create and queue a thread that will run that function
  ;; when started
  ($define! thread-create
    ($lambda (proc)
      ($let ((cont ($let/cc k k)))
        ($if (continuation? cont)
          (queue-thread cont)
          ($sequence
            (proc)
            (thread-join))))))

  ;; Syntax sugar to create and queue a thread; the body is the function to be
  ;; run when the thread starts
  ($define! $thread
    ($macro body
      (list thread-create (list* $lambda (list) body))))

  ;; Start the first thread in the thread pool and wait until all threads have
  ;; terminated
  ($define! thread-join
    ($lambda ()
      ($when (has-pending-threads)
        (apply-continuation (dequeue-thread) (list :thread-join))))))
