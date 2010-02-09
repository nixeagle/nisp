(asdf:defsystem :nisp.util.json
  :depends-on (:usocket
               :nisp.util.usocket
               :cl-json
               :with-fbound)
  :components
  ((:file "util.json")))