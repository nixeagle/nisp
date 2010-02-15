(in-package :nisp.i)

(define-new-suite :nisp-eos-root)
(def-suite nisp.i :in :nisp-eos-root)
(def-suite root :in :nisp-eos-root)

(defgeneric samep (arg1 arg2)
  (:documentation "Compare ARG1 to ARG2 based on their types."))
(defmethod samep (arg1 arg2)
  (values (funcall (eos::find-predicate arg1 arg2) arg1 arg2)
          (eos::find-predicate arg1 arg2)))

(test (ensure-string :suite :nisp-eos-root)
  (unwind-protect
       (with-fbound (ensure-string)
         ("a") "a"
         (:a) "A"
         ('a) "A"
         ('|a|) "a")
    (unintern :a :keyword)
    (unintern 'a)
    (unintern '|a|)))


;;;{{{ Comchar

(test (valid-comchar-string-p :suite root)
  (with-fbound (valid-comchar-string-p)
    "Non string is not a `comchar'."
    (1) nil
    "String of 0 length is not a `comchar'."
    ("") nil
    "String with a letter is not a `comchar'."
    ("a") nil
    "String with multiple letters is not a `comchar'."
    ("aa") nil
    "The string \"!\" is a `comchar'."
    ("!") t
    "Multiple ! is not a `comchar'."
    ("!!") nil))


(test (remove-comchar
       :suite root :depends-on valid-comchar-string-p)
  (with-fbound (remove-comchar)
    ("!" "!hi") "hi"
    "Number 1 is not a valid type"
    ("1" "1hi") :signals type-error
    "Should not be an applicable method for integer 1."
    (1 "1hi") :signals simple-error
    "If `comchar' is not first char in the message."
    ("!" "hi") nil
    ("!" "hi!") nil
    ("!!" "!!hi") :signals type-error
    ((make-instance 'comchar :comchar #\@) "@command") "command"))

;;;}}}

;;;{{{ irc packages
(def-suite nisp.i.irc-packages :in nisp.i)
(in-suite nisp.i.irc-packages)

(test (normalize-network-name :suite nisp.i.irc-packages)
  (with-fbound (normalize-network-name)
    "Dots should convert to dashes."
    ("irc.foo.net") "IRC-FOO-NET"

    "Do not do anything if no dots exist."
    ("nodots") "NODOTS"))

(test (ensure-network-package :suite nisp.i.irc-packages)
  (let ((expected-package :nisp.i.packages.irc-eighthbit-net)
        (fully-normallized :nisp-i-packages-irc-eighthbit-net))
    (unwind-protect
         (with-fbound (ensure-network-package)
           "Pass a string with a keyword argument."
           ("IRC.EIGHTHBIT.NET" :prefix :nisp.i.packages.)
           :satisfies packagep
           "No need to pass the prefix, should default."
           ("IRC.EIGHTHBIT.NET") :satisfies packagep
           "Pass two keyword args"
           (:irc.eighthbit.net :prefix :nisp.i.packages.)
           (find-package expected-package)

           "Prefix is allowed to be a string."
           (:irc.eighthbit.net :prefix "NISP.I.PACKAGES.")
           (find-package expected-package)

           "A keyword arg for :prefix is not required."
           (fully-normallized)
           :satisfies packagep))
      (and (find-package expected-package)
           (delete-package expected-package))
      (and (find-package fully-normallized)
           (delete-package fully-normallized))))
;;;}}}


;;;{{{ Command routing
(def-suite command-routing
    :description "Routing string messages."
    :in nisp.i)

(test (split-command-string :suite command-routing)
  (with-fbound (split-command-string)
    "Expect the string with no spaces in a list."
    ("Hi") '("Hi")
    "Expect a list with the 4 words as elements."
    ("Hi how are you") '("Hi" "how" "are" "you")
    (" Hi how are you") '("Hi" "how" "are" "you")
    ("Hi how are you ") '("Hi" "how" "are" "you")
    "Splitting the empty string."
    ("") '()))

(test (%generate-interned-argument-symbols :suite command-routing)
  (unwind-protect
       (with-fbound (%generate-interned-argument-symbols)
         "Result should be a list of symbols of length ARG1."
         (0) '()
         (1) '(ARG1)
         (3) '(ARG1 ARG2 ARG3)
         (2 "A~A") '(A1 A2))))

(test (%find-command-handler-generic :suite command-routing)
  (dotimes (n (length +command-routing-functions+))
    (with-fbound (%find-command-handler-generic)
      "Make sure lookup returns correct generic function."
      (n) (aref +command-routing-functions+ n))))

(test (find-command-handler-generic :suite command-routing)
  (with-fbound (find-command-handler-generic)
    "Passing multiple strings."
    ("Hi") (%find-command-handler-generic 1)
    ("Hi" "hi") (%find-command-handler-generic 2)
    "Passing lists of strings."
    ('()) (%find-command-handler-generic 0)
    ((list "Hi")) (%find-command-handler-generic 1)
    ((list "Hi" "hi")) (%find-command-handler-generic 2)
    "Passing a number"
    (0) (%find-command-handler-generic 0)
    (1) (%find-command-handler-generic 1)
    (2) (%find-command-handler-generic 2)))

(test (filter-to-remaining-arguments :suite command-routing)
  (with-fbound (filter-to-remaining-arguments)
    ("aaaa a" "aaaa") "a"
    ("test a" (list "test")) "a"
    ("test" "t") "est"
    ("test" "") "test"
    ("test a" (list 'NISP.I.COMMAND-ARGUMENT-SYMBOLS::|test|)) "a"

    "More then one command argument"
    ("test arg1 arg2" "test arg1") "arg2"
    ("test arg1 arg2" (list "test" "arg1")) "arg2"
    "No error when sequences are the same size"
    ("test" "test") ""))
;;;}}}

;;;{{{ Testing the "test-generic-function"
(in-package :nisp.i-zeta)
(def-suite root :in :nisp-eos-root)
(def-suite test-generic-function-suite :in root)
(test (test-gf :suite test-generic-function-suite)
  (with-fbound (test-gf)
    ("a" "a" "a") "aaa"
    ("a" 1 1) 2
    (1 1 1) :satisfies listp
    (t t t) :should-be test-method))

(in-package :nisp.i)
;;;}}}


;;;{{{ command funcallable class
(def-suite command-funcallable-class :in :nisp-eos-root)

;;;{{{ Testing direct operations on specializers
(test (available-specializers :suite command-funcallable-class)
  (is (eq (find-class 'closer-mop:eql-specializer)
          (car (available-specializers #'documentation)))
      "Should be getting a `eql-specializer' class.")
  (is (eq (find-class 'command-specializer)
          (car (available-specializers
                (make-instance 'command-generic-function))))))

(test (make-instance/command-specializer :suite command-funcallable-class)
  "Want a `command-specializer' type."
  (is (typep (make-instance 'command-specializer) 'command-specializer)))

(test (make-instance/command-generic-function :suite command-funcallable-class)
  "Want a `command-generic-function' type."
  (is (typep (make-instance 'command-generic-function) 'command-generic-function)))


(test (add-or-remove-direct-method
         :suite command-funcallable-class
         :depends-on make-instance/command-specializer)
  (let ((specializer (make-instance 'command-specializer)))
    "Verify that the following functions work correctly:~
      - `closer-mop:add-direct-method'~
      - `closer-mop:remove-direct-method'~
      - `closer-mop:specializer-direct-methods'."
    (is (= 4 (car (closer-mop:add-direct-method specializer 4)))
        "Verify we actually push something onto the list.")
    (is (eq :a (car (closer-mop:add-direct-method specializer :a)))
        "Make sure we can push another item on.")
    (is (equal (list :a 4) (closer-mop:specializer-direct-methods specializer))
        "Validate that `specializer-direct-methods' returns the right list.")
    (is (= 3 (length (closer-mop:add-direct-method specializer :b)))
        "By now we should have 3 items on our list")
    (is (= 3 (length (closer-mop:add-direct-method specializer :b)))
        "Adding the same item again should not make the list any longer.")
    (is (= 2 (length (closer-mop:remove-direct-method specializer :b)))
        "Removing an item should take us back down to two items.")
    (is (equal (list :a 4) (closer-mop:remove-direct-method specializer :b))
        "Removing the same item again should return us whats left with~
         no additional modifications.")
    (is (eq :a (car (closer-mop:remove-direct-method specializer 4)))
        "Removing 4 from the list should leave us with just :a left.")
    (is (null (closer-mop:remove-direct-method specializer :a))
        "Removing the final element should give us a `null' list.")))

(test (equal-really-is-equal-on-lists :suite command-funcallable-class)
  "Just checking my sanity, if this test fails, we are foobar'ed."
  (is (equal (list :a :b :c) (list :a :b :c))
      "Lists with the same elements should be `equal' to each other."))

(test (get-command-specializer-obarray :suite command-funcallable-class)
  (is (typep (get-command-specializer-obarray) 'hash-table)))

(test (ensure-command-symbol :suite command-funcallable-class)
  "No matter what symbol is passed in, we should be getting a symbol~
   that is interned in `:nisp.i.command-argument-symbols'."
  (is (eq 'nisp.i.command-argument-symbols::testingit
          (ensure-command-symbol 'testingit)))
  (is (equal
       '(NISP.I.COMMAND-ARGUMENT-SYMBOLS::A NISP.I.COMMAND-ARGUMENT-SYMBOLS::B)
       (ensure-command-symbol '(a b)))))

(test (add/remove-command-specializer
       :suite command-funcallable-class
       :depends-on (and . (ensure-command-symbol
                           get-command-specializer-obarray)))
  (is (typep (%add-command-specializer (get-command-specializer-obarray)
                                       (ensure-command-symbol 'testingit)
                                       (list (ensure-command-symbol 'testingit)))
             'command-specializer)
      "Add symbol testingit, which represents a command specializer.")
  (is (typep (%get-command-specializer (get-command-specializer-obarray)
                                       (ensure-command-symbol 'testingit))
             'command-specializer)
      "Using the raw getter method we should be getting the specializer~
      that we added in the last test.")
  (is (typep (find-command-specializer (ensure-command-symbol 'testingit))
             'command-specializer)
      "Now use the userlevel `find-command-specializer' to do the same~
      thing as the last test.")
  (is (typep (%add-command-specializer
              (command-specializer-direct-nodes
               (find-command-specializer (ensure-command-symbol 'testingit)))
               (ensure-command-symbol 'testingit2)
               (list  (ensure-command-symbol 'testingit)
                       (ensure-command-symbol 'testingit2)))
             'command-specializer))
  (with-fbound (find-command-specializer)
    "Given a list, we should get the specializer for:~
     testingit -> testingit2."
    ((list (ensure-command-symbol 'testingit)
           (ensure-command-symbol 'testingit2)))
    :should-be command-specializer
    "An empty list does not specialize on anything, so return `nil'."
    ('()) nil
    "A list with one symbol should return the right specializer."
    ((list (ensure-command-symbol 'testingit)))
    :should-be command-specializer
    "A single symbol should return the specializer on the top level."
    ((ensure-command-symbol 'testingit))
    :should-be command-specializer
    "A symbol referring to a command below the top level returns `nil'."
    ((ensure-command-symbol 'testingit2))
    nil)
  (let* ((top-level (intern-command-specializer 'testingit))
         (second-level (intern-command-specializer '(testingit testingit-again))))
   (with-fbound (intern-command-specializer)
     ('testingit) top-level
     ('(testingit testingit-again)) second-level
     ('(testcase testcase testcase hi))
     (intern-command-specializer '(testcase testcase testcase hi))
     ; "The number or symbol 1 should intern as just that."
     ; (1) :should-be command-specializer
      )

    (is (eq t (%remove-command-specializer
               top-level
               (ensure-command-symbol 'testingit-again))))
    (is (eq t (%remove-command-specializer
               (get-command-specializer-obarray)
               (ensure-command-symbol 'testingit)))
        "Because we added a specializer in the last test, this one should~
      return true when we try to remove it from the obarray.")))

;;;}}}

;;;{{{ Testing operations with fake command functions

(defgeneric fake-test-command (arg1 arg2 arg3 arg4 arg5)
  (:generic-function-class command-generic-function))
(defmethod fake-test-command ((a integer)
                              (b integer) (c string)
                              (d (eql 4)) (f (eql 3)))

  (list a b c d f))



(test (make-method-specializers-form
       :suite root)
  (let* ((gf #'fake-test-command)
         (method (car (closer-mop:generic-function-methods gf)))
         (test-specializer (intern-command-specializer '(testing it))))
    (flet ((make-specializers-form (&rest forms)
             (sb-pcl:make-method-specializers-form gf method forms nil)))
      (is (typep gf 'command-generic-function))
      (is (typep method 'command-method))
      (with-fbound (make-specializers-form)
        ((closer-mop:intern-eql-specializer 3))
        `(list ,(closer-mop:intern-eql-specializer 3))
        ('(eql 3))
        '(list (sb-mop:intern-eql-specializer 3))
        ((find-class 'integer))
        `(list ,(find-class 'integer))
        ('integer)
        '(list (find-class 'integer))
        ((intern-command-specializer '(testing it)))
        `(list ,test-specializer)
        ('(command testing it))
        `(list ,test-specializer)
        ('(command testing it) '(eql 3))
        `(list ,test-specializer (sb-mop:intern-eql-specializer 3))
        ('integer '(command testing it))
        `(list (find-class 'integer) ,test-specializer)))))

#+ () (test (fake-test-command)
  (is (eq nil)))

;;;}}} fake command functions

;;;}}} command funcallable class

;;;{{{ Silly demo that needs moved somewhere else
#+ ()
(test demo
  (with-fbound (+)
    "3 numbers add to 6"
    (1 2 3) 6
    "1 by itself is 1"
    (1) 1
    "Adding 1 2 and 3 should not give me 5."
    (1 2 3) (not 5)
    "Result is always of type integer integer."
    (1 2 3) :should-be integer
    (1 2 3) :expects integer
    "Result can also be tested in terms of `not' being of a type."
    (1 2 3) :should-be (not string)
    "Result is checked with the _function_ `integerp'.
     This predicate/function should take one argument."
    (1 2 3) :satisfies integerp
    (1 2 3) :should-satisfy integerp
    "Result can also be tested in terms of `not' matching the predicate."
    (1 2 3) :satisfies (not stringp)
    "No arguments should be 0"
    () 0
    ("a string")
    :signals error
    ("another one")
    :should-signal error
    "Addition should always run to completion"
    (1 2 3) :finishes))
;;;}}}