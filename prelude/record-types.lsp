;;;; Defining new record types, encapsulation types that contain mutable fields.

($provide! ($declare-record-type!)
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
  ($define! $declare-record-type!
    ($vau (name fields) e
      ($define! (raw-capsulate raw-test? raw-decapsulate) (make-encapsulation-type))

      ($define! constructor-name name)
      ($define! predicate-name (join-symbol name "?"))
      ($define! getter-names (map ($lambda (field) (join-symbol name "-" field)) fields))
      ($define! setter-names (map ($lambda (field) (join-symbol "set-" name "-" field "!")) fields))

      ;; A lambda that takes its arguments, turns them into an environment, and
      ;; encapsulates the result.
      ($define! constructor
        ;; sample constructor:
        ;; ($lambda (x y)
        ;;   (raw-capsulate ($bindings->environment (x x) (y y))))
        ($let*
          ((bindings (zip-with list fields fields)) ; e.g. ((x x) (y y))
           (record-env (cons $bindings->environment bindings)))
          (apply (wrap $lambda)
                 (list
                   fields
                   (list raw-capsulate record-env)))))
      (apply (wrap $set!) (list e constructor-name constructor))

      ;; Predicate to test that all arguments are of the correct type
      ($define! test?
        ($lambda xs
          (apply (wrap $and?)
                 (map ($lambda (x)
                        (raw-test? x))
                      xs))))
      (apply (wrap $set!) (list e predicate-name test?))

      ;; Getters
      (for-each (zip fields getter-names)
        ($lambda ((field . getter-name))
          ($let
            ((getter
               ($lambda (r)
                 (apply (wrap $get) (list (raw-decapsulate r) field)))))
            (apply (wrap $set!) (list e getter-name getter)))))

      ;; Setters
      (for-each (zip fields setter-names)
        ($lambda ((field . setter-name))
          ($let
            ((setter
               ($lambda (r value)
                 (apply (wrap $set!) (list (raw-decapsulate r) field value))
                 #inert)))
            (apply (wrap $set!) (list e setter-name setter))))))))
