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
    ('it) (list (find-symbol "IT" +tree-symbols-package+))
    ('(it it2)) (list (find-symbol "IT" +tree-symbols-package+)
                      (find-symbol "IT2" +tree-symbols-package+))
    ("IT") (list (find-symbol "IT" +tree-symbols-package+))
    ("it") (list (find-symbol "IT" +tree-symbols-package+))
    ("it it2") (list (find-symbol "IT" +tree-symbols-package+)
                     (find-symbol "IT2" +tree-symbols-package+))))
