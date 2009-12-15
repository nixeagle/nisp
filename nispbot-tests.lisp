(in-package :nispbot)

(deftestsuite root-suite (nisp::root-suite) ())
(deftestsuite basic-irc-suite (root-suite) ())

(deftestsuite strip-newline (root-suite)
  ()
  (:test (pass-string
          (:documentation "Base case")
          (ensure (stringp (strip-newline "some string"))))))

(deftestsuite test-make-irc-message (basic-irc-suite)
  ()
  (:test (format-private-message
           (:documentation "Demonstrate how to create a private message and
verify that the result is correct.")
           (ensure-same (irc::make-irc-message "privmsg" "#channel" "text")
                        (format nil "privmsg #channel :text~%")
                        :test #'string=))))

(deftestsuite test-tokenize-string (basic-irc-suite)
  ()
  (:documentation "Demonstrate irc::tokenize-string")
  (:test (split-by-space
          (ensure-same (irc::tokenize-string
                        "Some string to test on"
                        :delimiters '(#\Space))
                       '("Some" "string" "to" "test" "on")))))