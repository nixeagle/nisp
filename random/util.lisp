;;;; Extra functions

(in-package :nisp-util)

;;; For the lack of any better place to put these for now...
;(setq *test-describe-if-not-successful?* t)

(deftestsuite nisp-util (nisp::root-suite) ())

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

(deftestsuite count-symbols (nisp-util)
  ((i (count-symbols :nisp-util)))
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

(defun strip-newlines (string)
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (coerce
   (loop for char in (coerce string 'list)
      if (not (eq #\Newline  char))
      collect char)
   'string))

(deftestsuite strip-newlines (nisp-util)
  ()
  (:test (pass-string
          (:documentation "Base case")
          (ensure (stringp (strip-newlines "some string"))))))

;;; end file