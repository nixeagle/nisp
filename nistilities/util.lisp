;;;; Extra functions

(in-package :nistilities)

;;; For the lack of any better place to put these for now...
;(setq *test-describe-if-not-successful?* t)

(deftestsuite root-suite () ())
(deftestsuite nistilities (root-suite) ())

(defvar *printable-ascii-characters*
  ;; Thanks to baddog of eighthbit.net for generating these.
  (list #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.
        #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\=
        #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
        #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[
        #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
        #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
        #\z #\{ #\| #\} #\~)
  "Printable ASCII chars")

(defun ascii-character-range (start end)
  "Iterate over the printable ASCII chars and return a list of the subset."
  (loop for char in *printable-ascii-characters*
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

(deftestsuite count-symbols (nistilities)
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

(deftestsuite test-strip-newlines (nistilities)
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

;;; end file