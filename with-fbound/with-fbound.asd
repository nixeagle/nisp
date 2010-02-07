(asdf:defsystem :with-fbound
  :license "MIT"
  :depends-on (:Eos :iterate)
  :components
  ((:file "check")
   (:file "with-fbound" :depends-on ("check"))))