(asdf:defsystem :nisp.util.json
  :depends-on (:usocket
               :json
               :with-fbound)
  :components
  ((:file "util.json")))