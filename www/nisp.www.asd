(asdf:defsystem :nisp.www
  :depends-on (:hunchentoot
               :nisp.i
               ;; dependent on me
               :nisp.global
               )
  :components
  ((:file "hunchentoot-alpha")))