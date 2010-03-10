(asdf:defsystem :php-serialization
  :components
  ((:file "php-serialization")))

(asdf:defsystem :php-serialization-tests
  :depends-on (:eos :php-serialization)
  :components
  ((:file "php-serialization-tests")))

;;; END