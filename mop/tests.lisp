(in-package :nisp.mop-simple)
(define-new-suite :nisp-eos-root)
(def-suite root :in :nisp-eos-root)

(test (class-slots :suite root)
  "Result of `class-slots' should be list of `standard-effective-slot-definition'."
  (mapc (lambda (class)
          (is (typep
               (car (class-slots class)) '
               closer-mop:standard-effective-slot-definition)))
        (list 'standard-class
              "STANDARD-CLASS")))