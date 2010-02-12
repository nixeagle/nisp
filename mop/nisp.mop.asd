(asdf:defsystem :nisp.mop
  :depends-on (:closer-mop
               :alexandria
               :iterate
               :with-fbound)
  :components ((:file "package")
               (:file "simple")
               (:file "mop")
               (:file "store")
               (:file "tests")))