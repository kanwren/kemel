;;;; Defining new record types

($provide! (record? $declare-record!)
  ($define!
    (record-capsulate record? record-decapsulate)
    (make-encapsulation-type))

  ;; Concatenate a list of strings or symbols into a symbol
  ($define! join-symbol
    ($lambda xs
      (string->symbol
        (apply string-append
               (map ($lambda (x)
                      ($if (symbol? x) (symbol->string x) x))
                    xs)))))

  ;; Declare a record into the current environment.
  ;;
  ;; Internally, a record is an encapsulation containing an environment with a
  ;; binding for each field. Lookup and mutation are performed via $get and $set
  ;; respectively.
  ($define! $declare-record!
    ($vau (name fields) e
      ($define! (raw-capsulate raw-test? raw-decapsulate) (make-encapsulation-type))

      ;; Predicate to test that all arguments are of the correct type
      ($define! test?
        ($lambda xs
          (apply (wrap $and?)
                 (map ($lambda (x)
                        ($and? (record? x)
                               (raw-test? (record-decapsulate x))))
                      xs))))

      ($let
        ((test-name (join-symbol name "?")))
        (apply (wrap $set!) (list e test-name test?)))

      ;; A lambda that takes its arguments, turns them into an environment, and
      ;; encapsulates the result.
      ($define! constructor
        ;; sample constructor:
        ;; ($lambda (x y)
        ;;   (record-capsulate (raw-capsulate ($bindings->environment (x x) (y y)))))
        ($let*
          ((bindings (zip-with list fields fields)) ; e.g. ((x x) (y y))
           (record-env (cons $bindings->environment bindings)))
          (apply (wrap $lambda)
                 (list
                   fields
                   (list record-capsulate (list raw-capsulate record-env))))))

      (apply (wrap $set!) (list e name constructor))

      (for-each fields
        ($lambda (field)
          ;; Generate getters
          ($let
            ((getter-name (join-symbol name "-" field))
             (getter
               ($lambda (r)
                 (apply (wrap $get) (list (raw-decapsulate (record-decapsulate r)) field)))))
            (apply (wrap $set!) (list e getter-name getter)))
          ;; Generate setters
          ($let
            ((setter-name (join-symbol "set-" name "-" field "!"))
             (setter
               ($lambda (r value)
                 (apply (wrap $set!) (list (raw-decapsulate (record-decapsulate r)) field value))
                 #inert)))
            (apply (wrap $set!) (list e setter-name setter))))))))
