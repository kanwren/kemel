;;; TODO: make appropriate adjustments once operand trees are implemented for
;;; continuations

($provide! (thread-yield thread-create thread-end)
  ($define! thread-list ())
  ($define! pending-threads ($lambda () (not? (null? thread-list))))

  ($define! env (get-current-environment))
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
      (dequeue-thread)))

  ($define! thread-yield
    ($lambda ()
      ($when (pending-threads)
        ($let ((cont ($let/cc k k)))
          ($when (continuation? cont)
            (apply-continuation (swap-thread cont) (list :thread-yield)))))))

  ($define! thread-create
    ($lambda (proc)
      ($let ((cont ($let/cc k k)))
        ($if (continuation? cont)
          (queue-thread cont)
          ($sequence
            (proc)
            (thread-end))))))

  ($define! thread-end
    ($lambda ()
      ($unless (null? thread-list)
        (apply-continuation (dequeue-thread) (list :thread-end))))))
