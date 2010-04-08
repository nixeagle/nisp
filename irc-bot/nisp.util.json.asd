(asdf:defsystem :nisp.util.json
  :depends-on (:usocket
               :nisp.util.usocket
               :cl-json
               :eos
               )
  :components
  ((:file "util.json")))