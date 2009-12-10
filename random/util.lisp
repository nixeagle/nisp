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