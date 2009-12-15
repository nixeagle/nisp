;;;; Extra functions

(in-package :nistilities)

;;; For the lack of any better place to put these for now...
;(setq *test-describe-if-not-successful?* t)


(deftestsuite nistilities-suite () ())

(define-constant +printable-ascii-characters+
  ;; Thanks to baddog of eighthbit.net for generating these.
  (list #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.
        #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\=
        #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
        #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[
        #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
        #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
        #\z #\{ #\| #\} #\~)
  "Printable ASCII chars. This is defined as a constant list for now because I cannot figure out how to change from an ascii character code to the character representation.")

(defun ascii-character-range (start end)
  "Iterate over the printable ASCII chars and return a list of the subset."
  (loop for char in +printable-ascii-characters+
         when (and (char<= start char)
                   (char>= end char)) collect char))

;;;This is a terrible implentation, but I cannot figure out how to tell
;;;the compiler that I want to simply iterate and count instead of
;;;consing and making a list of symbols then counting it. This will do
;;;until a better implentation is found. The primary use for it is in
;;;unit tests not primary use.
(defun count-symbols (&optional name)
  "Count the number of symbols found in package NAME.

If NAME is not given assume *package* instead"
  (let ((package (or (find-package name) *package*)))
    (length (loop 
            for s being the symbols of (find-package package)
            collect s))))

(deftestsuite count-symbols (nistilities-suite)
  ((i (count-symbols :nistilities)))
  (:test (is-integer (ensure (integerp i))))
  (:test (is-positive (ensure (<= 0 i))))
  (:run-setup :once-per-suite)
;  (:run-setup :once-per-session)
  (:documentation "
Portably count the number of all symbols in a given package.
"))


(defmacro ensure-predicate (predicate form &optional format &rest args)
  "Using predicate, throw an error if the result is not true
but make sure to explain what went wrong in the format string."
;  (list 'nyi predicate form format args)
  (let ((gpredicate (gensym))
        (result (gensym)))
    `(let ((,gpredicate #',predicate))
       (let ((result (funcall ,gpredicate ,form)))
         (if 
          t
          (list ',predicate ',form ,form)))))
  )

(ensure-predicate integerp (string "abc") )
;(ensure (foobar a) )

(defun strip-newlines (string &optional (replace-char nil))
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (coerce
   (loop for char in (coerce string 'list)
      when (and replace-char (eq char #\Newline)) collect replace-char
      unless (eq char #\Newline) collect char)
   'string))

(deftestsuite test-strip-newlines (nistilities-suite)
  ()
  (:test (pass-string
          (:documentation "Base case")
          (ensure (stringp (strip-newlines "some string")))))
  (:test (on-a-string-with-newlines
          (:documentation "With new lines. Please do not fix the newline in this test.")
          (ensure-same (strip-newlines "String
Newline") "StringNewline")))
  (:test (remove-newlines-leave-space
          (:documentation "Make sure we can replace newlines with another char")
          (ensure-same (strip-newlines "String
Newline" #\Space)
                       "String Newline"))))

(defgeneric range (start end)
  (:documentation "Generate a list of integers from start to end."))
(defmethod range ((start number) (end number))
  (if (> start end)
      (loop for x from start downto end collect x)
      (loop for x from start to end collect x)))

(defmethod range ((start string) (end string))
  (unless (= 1 (length start) (length end))
    (error "Cannot represent strings with length > 1 as a character."))
  (range (character start) (character end)))

(defmethod range (start end)
  (if (char>= start end)
      (nreverse (ascii-character-range end start))
      (ascii-character-range start end)))

(deftestsuite test-range (nistilities-suite)
  ()
  (:documentation
   "Perform tests on range convenience function. In general it should handle any useful range.")
  (:function
   (run-range (start end)
              (ensure (cl:listp (range start end)))
              (ensure (cl:< 0 (cl:list-length (range start end))))))
  (:test (count-up
          (:documentation "The usual case, counting from START to END")
          (ensure-same (range 0 10)
                       (cl:list 0 1 2 3 4 5 6 7 8 9 10))))
  (:test (count-down
          (:documentation
           "A tad unusual, but we expect given a START greater then END
to count _down_ from START to END.")
          (ensure-same (range 5 -5)
                       (cl:list 5 4 3 2 1 0 -1 -2 -3 -4 -5))))
  (:test (random-integer-combos
          (:documentation
           "Playing around with random test cases. These are largely
undocumented in the lift test framework.")
          (lift:ensure-random-cases 10
              ((start an-integer)
               (end an-integer))
            (run-range start end))))
  (:test (random-float-combos
          (:documentation
           "Make sure that no combination of floats break range.")
          (lift:ensure-random-cases 10
              ((start a-double-float)
               (end a-single-float))
            (run-range start end))))
  (:test (count-up-with-char
          (:documentation "Count up from a lower START char to a higher
END char.")
          (ensure-same (range #\a #\c)
                       (cl:list #\a #\b #\c))))
  (:test (count-down-from-char
          (:documentation "Count down from a higher START to a lower END
char.")
          (ensure-same (range #\c #\a)
                       (cl:list #\c #\b #\a))))
  (:test (count-down-from-char-passing-strings
          (:documentation "Passing two one letter strings is valid input")
          (ensure-same (range "c" "a")
                       (cl:list #\c #\b #\a)))))


;;; end file
