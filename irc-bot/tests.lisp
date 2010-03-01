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

;;; END