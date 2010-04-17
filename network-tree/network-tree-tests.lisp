;;; Tests for tree-funcallable-class
(in-package :nisp.network-tree)

#+ ()
(progn
  (fmakunbound 'test-tree-generic-function)
  (defgeneric test-tree-generic-function (tree arg1)
    (:generic-function-class network-tree-generic-function))
  (defmethod test-tree-generic-function ((tree (eql "hi")) arg1)
    (list tree arg1 (next-node)))
  (defmethod test-tree-generic-function ((tree (eql "hi there")) arg1)
    (list tree (and (> arg1 1) (remaining-parameters))))
  (defmethod test-tree-generic-function ((tree (eql "hi there")) (arg1 integer))
    (list tree "ARG1 was an integer" (call-next-method)))
  (defmethod test-tree-generic-function ((tree (eql "hi there")) (arg1 (eql 10)))
    (list tree "ARG1 was = to 10" (call-next-method)))
  (defmethod test-tree-generic-function :around ((tree (eql "hi there"))
                                                 arg1)
    (list tree "around!" (call-next-method)))

  (defmethod test-tree-generic-function ((tree (eql "hi there hi")) arg1)
    (list tree (and (> arg1 2) (next-node))))
  (defmethod test-tree-generic-function ((tree (eql "hi there hi how")) arg1)
    (list tree (and (> arg1 3) (next-node))))
  (defmethod test-tree-generic-function ((tree (eql "hi there hi how are")) arg1)
    (list tree (and (> arg1 4) (next-node))))
  (defmethod test-tree-generic-function ((tree (eql "hi there hi how are you")) arg1)
    (list tree (and (> arg1 5) (next-node))))
  (defmethod test-tree-generic-function ((tree (eql "hi there hi how are you doing")) arg1)
    (list tree arg1 (remaining-parameters)))



  (defmethod test-tree-generic-function :before ((tree (eql "hi there hi")) arg1)
    1))

(progn
  (fmakunbound 'test-tree-generic-function)
  (fmakunbound 'call-test-tree-generic-function)
  (defgeneric test-tree-generic-function (tree arg1)
    (:generic-function-class network-tree-generic-function))
  (defun call-test-tree-generic-function (tree arg1)
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (simple-string tree))
    (test-tree-generic-function (tree-generic-direct-node *network-tree-nodes* tree) arg1))
  (defmethod test-tree-generic-function ((tree (eql "hi")) arg1)
    arg1
    "hi"
#+ ()    (remaining-parameters))
  (defmethod test-tree-generic-function ((tree (eql "one")) arg1)
    (next-node))
  (defmethod test-tree-generic-function ((tree (eql "one two")) arg1)
    (next-node))
  (defmethod test-tree-generic-function ((tree (eql "one two three")) arg1)
    arg1)
#+ ()  (defmethod test-tree-generic-function ((tree (eql (intern-network-tree-node "hi"))) arg1)
    arg1
    "hi"))





(def-suite root)

(test (first-command-word :suite root)
  (is (string= "Hi" (first-command-word "Hi how are you?")))
  (is (string= "Hi h" (first-command-word "Hi how are you?" #\o))
      "Should be splitting on #\o.")
  (is (string= "" (first-command-word ""))
      "Empty string when no more words left."))

(test (network-node-string-p :suite root)
  "Predicate returns false if a string has a space in it.

We assume input is a string."
  (is (eq t (network-node-string-p "hi")))
  (is (eq nil (network-node-string-p "hi there")))
  (is (null (network-node-string-p " hi "))
      "Leading or trailing spaces are _not_ ok if there is only one
      \"word\" given as we should be able to parse out that and convert it
      to a symbol."))

(test (ensure-network-node-symbol :suite root)
  #+ ()
  (with-fbound (ensure-network-node-symbol)
    ('it) (find-symbol "IT" +network-tree-symbols-package+)
    ('(it it2)) :signals error
    ("IT") (find-symbol "IT" +network-tree-symbols-package+)
    "Lowercase input should result in an uppercase symbol."
    ("it") (find-symbol "IT" +network-tree-symbols-package+)
    "Strings with spaces in them should error as it makes no sense to make
a single tree symbol out of what boils down to one symbol"
    ("it it2") :signals error))

(test (ensure-network-node-symbols :suite root
                                   :depends-on ensure-network-node-symbol)
  #+ ()
  (with-fbound (ensure-network-node-symbols)
    ('(hi how)) '(:HI :HOW)
    ("hi how") '(:HI :HOW)
    ('hi) '(:HI)))


(test (intern-network-tree-node/typep :suite root)
  "All that matters is we get a tree specializer"
  (is (typep (intern-network-tree-node '("hi"))
             'network-tree-node)))

(test (intern-network-tree-node/same/one-level
       :suite root
       :depends-on intern-network-tree-node/typep)
  "The result from interning should always be the same object"
  (let ((expected (intern-network-tree-node '(:HI))))
    (is (eq expected (intern-network-tree-node '(:HI))))
    (is (eq expected (intern-network-tree-node '("hi"))))
    (is (eq expected (intern-network-tree-node "hi")))
    (is (eq expected (intern-network-tree-node :hi)))))

(test (intern-network-tree-node/same/two-levels
       :suite root
       :depends-on intern-network-tree-node/same/one-level)
  (let ((expected (intern-network-tree-node '(:TWO :LEVELS))))
    (is (eq expected (intern-network-tree-node '(:TWO :LEVELS))))
    (is (eq expected (intern-network-tree-node '("two" "levels"))))
    (is (eq expected (intern-network-tree-node "two levels")))))

(test (test-tree-generic-function/complex-method-combination
       :suite root
       :depends-on intern-network-tree-node)
  (finishes (test-tree-generic-function "hi2 there" 10)))
;;; END
