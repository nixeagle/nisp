(asdf:defsystem :nisp.www
  :depends-on (:hunchentoot
               :parenscript
               :cl-who
               :nisp.i
               ;; dependent on me
               :nisp.global
               )
  :components
  ((:file "hunchentoot-alpha")
   (:file "ninthbit")))