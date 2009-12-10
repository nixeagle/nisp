(in-package :nisp-random)

;;; source taken from swank and modified to do what I wanted to do.

(defun matches-list (reg &rest strings)
  (let ((ret ()))
    (dolist (str strings ret)
      (push 
       (second (multiple-value-list 
                (cl-ppcre:scan-to-strings reg str))) ret))))