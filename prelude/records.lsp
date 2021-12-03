;;;; Defining new records.
;;;;
;;;; A "record" is an encapsulation that contains a "record type", an
;;;; encapsulation with fields. The primary distinction is that all declared
;;;; records are subtypes of the "record" encapsulation type, and can therefore
;;;; be tested for via "record?".

($provide! (record? $declare-record!)
  ($define!
    (record-capsulate record? record-decapsulate)
    (make-encapsulation-type))

  ($define! join-symbol
    ($lambda xs
      (string->symbol
        (apply string-append
               (map ($lambda (x)
                      ($if (symbol? x) (symbol->string x) x))
                    xs)))))

  ($define! $declare-record!
    ($vau (name fields) e
      ($define! (raw-capsulate raw-test? raw-decapsulate) (make-encapsulation-type))

      ($define! constructor-name name)
      ($define! predicate-name (join-symbol name "?"))
      ($define! getter-names (map ($lambda (field) (join-symbol name "-" field)) fields))
      ($define! setter-names (map ($lambda (field) (join-symbol "set-" name "-" field "!")) fields))

      ($define! constructor
        ($let*
          ((bindings (zip-with list fields fields)) ; e.g. ((x x) (y y))
           (record-env (cons $bindings->environment bindings)))
          (apply (wrap $lambda)
                 (list
                   fields
                   (list record-capsulate (list raw-capsulate record-env))))))
      (apply (wrap $set!) (list e constructor-name constructor))

      ($define! test?
        ($lambda xs
          (apply (wrap $and?)
                 (map ($lambda (x)
                        ($and? (record? x)
                               (raw-test? (record-decapsulate x))))
                      xs))))
      (apply (wrap $set!) (list e predicate-name test?))

     ;; Generate getters
     (for-each (zip fields getter-names)
       ($lambda ((field . getter-name))
         ($let
           ((getter
              ($lambda (r)
                (apply (wrap $get) (list (raw-decapsulate (record-decapsulate r)) field)))))
           (apply (wrap $set!) (list e getter-name getter)))))
     ;; Generate setters
     (for-each (zip fields setter-names)
       ($lambda ((field . setter-name))
         ($let
           ((setter
              ($lambda (r value)
                (apply (wrap $set!) (list (raw-decapsulate (record-decapsulate r)) field value))
                #inert)))
           (apply (wrap $set!) (list e setter-name setter))))))))
