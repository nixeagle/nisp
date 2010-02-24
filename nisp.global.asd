(asdf:defsystem :nisp.global
  :serial t
  :components
  ((:file "global-package")
   (:module "global"
            :components
            ((:file "system-id")))))