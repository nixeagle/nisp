;;;; Extra functions

(in-package :nisp-util)

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

(test count-symbols
  (is (<= 0 (count-symbols :nisp-util))))