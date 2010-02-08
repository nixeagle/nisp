(asdf:defsystem :nisp.util.json
  :depends-on (:usocket
               :cl-json
               :with-fbound)
  :components
  ((:file "util.json")))