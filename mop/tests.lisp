(in-package :nisp.mop-simple)
(def-suite root)

(test (class-slots :suite root)
  "Result of `class-slots' should be list of `standard-effective-slot-definition'."
  (mapc (lambda (class)
          (is (typep
               (car (class-slots class)) '
               closer-mop:standard-effective-slot-definition)))
        (list 'standard-class
              "STANDARD-CLASS")))