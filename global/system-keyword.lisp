(in-package :%global)

(defparameter +systems+
  '(("lappy" . :nisp-devel)
    ("box" . :nisp-vps))
  "Alist of common systems that nixeagle runs nisp on. These are lookups
  to get to a standardized keyword in some cases where these are
  required.")

(pushnew (cdr (assoc (machine-instance) +systems+ :test #'string=))
         *features*)

(defun system-keyword ()
  "Return a keyword depending on where we are running. If the system is
unknown, we return nil."
  (cdr (assoc (machine-instance) +systems+ :test #'string=)))