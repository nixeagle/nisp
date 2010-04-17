(asdf:defsystem :nisp.network-tree
  :depends-on (:alexandria :split-sequence :closer-mop :eos :iterate)
  :serial t
  :components
  ((:file "package")
   (:file "network-tree")
   (:file "network-tree-tests")))