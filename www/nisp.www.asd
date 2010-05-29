(asdf:defsystem :nisp.www
  :depends-on (:hunchentoot
               :hunchentoot-vhost
               :parenscript
               :cl-who
               :nutils
               :nisp.i
               ;; dependent on me
               :nisp.global
               )
  :serial t
  :components
  ((:file "hunchentoot-alpha")
   (:file "ninthbit")))
