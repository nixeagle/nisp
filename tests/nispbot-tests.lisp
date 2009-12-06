

(in-package :nispbot-tests)

(def-suite all-tests
    :description "Top level test suite")

(def-suite basic-irc
    :in all-tests
    :description "Tests all the irc related stuff")

(in-suite basic-irc)

(test (make-irc-message)
   "Testing usage of `cl-irc:make-irc-message'"
   (is (string=
        (format nil "privmsg #channel :text~%")
        (irc::make-irc-message "privmsg" "#channel" "text"))
       "Simplest case of a private message to a channel"))

(test (tokenize-string)
  "Demonstrate irc::tokenize-string"
  (is (equal (irc::tokenize-string
          "Some string to test on"
          :delimiters '(#\Space))
         '("Some" "string" "to" "test" "on"))))

(test parse-bot-command
  (is-every string=
    ((nispbot::parse-bot-command ",arglist +")
     "arglist")
    ((multiple-value-bind (ignore arg) (nispbot::parse-bot-command ",arglist xyz")
       arg)
     "xyz")))

(test (function-lambda-list-to-string :depends-on parse-bot-command)
  "Get a valid arglist with no errors."
  (is
   (string=
    
    "(&REST ARGS)"
    (nispbot::function-lambda-list-to-string "+")))
  (5am:finishes
    (nispbot::function-lambda-list-to-string "1")
    "Do better then throwing an error on non-function objects"))


(def-suite config-tests :in all-tests)
(in-suite config-tests)

(test *channel*-is-a-string
  "Make sure that we don't change the type without letting users know."
  (is (stringp nispbot-config::*channel*)))

(defpackage #:introspection-tests
  (:use ))

(def-suite introspection-tests
    :in all-tests
    :description "Test that introspection functions behave as they should")

(def-suite function-lambda-list/suite
    :in introspection-tests
    :description "Verify that lambda list generation is correct.")

(in-suite function-lambda-list/suite)

(test invalid-string
  (is (stringp ())))

(def-suite error-handling-tests :in all-tests)
(in-suite error-handling-tests)