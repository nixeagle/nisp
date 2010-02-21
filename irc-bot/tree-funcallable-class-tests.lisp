;;; Tests for tree-funcallable-class
(in-package :nisp.i)
(defgeneric test-tree-generic-function (tree arg1)
  (:generic-function-class tree-generic-function))

(defmethod test-tree-generic-function (tree arg1)
  (format nil "Catchall tree method: ~A ::arg1: ~A~%" tree arg1))


(define-new-suite :nisp-eos-root)

(def-suite tree-funcallable :in root)

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


(test (preprocess-arglist
       :suite tree-funcallable
       :depends-on ensure-tree-symbol)
  "Expect only first argument to ever be modified in the return value."
  (let ((expect '((NISP.I.COMMAND-ARGUMENT-SYMBOLS::ALPHA
                   NISP.I.COMMAND-ARGUMENT-SYMBOLS::NIKURL)
                  1 2 3 4 "args")))
    (with-fbound (preprocess-arglist)
      (#'test-tree-generic-function '("Alpha nikurl" 1 2 3 4 "args"))
      expect
      (#'test-tree-generic-function '((alpha nikurl) 1 2 3 4 "args"))
      expect)))