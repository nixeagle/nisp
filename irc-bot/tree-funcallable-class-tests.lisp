;;; Tests for tree-funcallable-class
(in-package :nisp.i)
(defgeneric test-tree-generic-function (tree arg1)
  (:generic-function-class tree-generic-function))

;;; Won't compile for a while because the first arg is not a network-tree.
#+ ()
(defmethod test-tree-generic-function (tree arg1)
  (format nil "Catchall tree method: ~A ::arg1: ~A~%" tree arg1))

(defmethod test-tree-generic-function ((tree (eql "hi")) arg1)

  #+ () (list (current-node)))
(progn
  (defmethod test-tree-generic-function ((tree (eql "hi2")) arg1)
    (list tree (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi2 there")) arg1)
    (list tree (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi2 there hi")) arg1)
    (list tree (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi2 there hi how")) arg1)
    (list tree (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi2 there hi how are")) arg1)
    (list tree (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi2 there hi how are you")) arg1)
    (list tree arg1 (remaining-parameters)))

  (defmethod test-tree-generic-function :before ((tree (eql "hi there hi")) arg1)
    1))


(define-new-suite :nisp-eos-root)
(define-new-suite 'root :in :nisp-eos-root)
(def-suite tree-funcallable :in root)

(test (tree-symbol-string-p :suite tree-funcallable)
  "Predicate returns false if a string has a space in it.

We assume input is a string."
  (is (eq t (tree-symbol-string-p "hi")))
  (is (eq nil (tree-symbol-string-p "hi there")))
  (is (eq t (tree-symbol-string-p " hi "))
      "Leading or trailing spaces are ok if there is only one \"word\"
      given as we should be able to parse out that and convert it to a
      symbol."))

(test (ensure-tree-symbol :suite tree-funcallable)
  (with-fbound (ensure-tree-symbol)
    ('it) (find-symbol "IT" +tree-symbols-package+)
    ('(it it2)) :signals error
    ("IT") (find-symbol "IT" +tree-symbols-package+)
    "Lowercase input should result in an uppercase symbol."
    ("it") (find-symbol "IT" +tree-symbols-package+)
    "Strings with spaces in them should error as it makes no sense to make
a single tree symbol out of what boils down to one symbol"
    ("it it2") :signals error))

(test (ensure-tree-symbols :suite tree-funcallable
                           :depends-on ensure-tree-symbol)
  (with-fbound (ensure-tree-symbols)
    ('(hi how)) '(:HI :HOW)
    ("hi how") '(:HI :HOW)
    ('hi) '(:HI)))


(test (intern-network-tree-node :suite tree-funcallable)
  (is (typep (intern-network-tree-node '("hi"))
             'network-tree-node)
      "All that matters is we get a tree specializer")
  (is (eq (intern-network-tree-node '("hi"))
          (intern-network-tree-node '("hi")))
      "The result from interning should always be the same object"))

;;; END
