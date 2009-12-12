(defpackage #:nisp-dev-helper
  (:use :cl :nispbot :trivial-shell :lift :tinaa))

(in-package :nisp-dev-helper)

(defun generate-html-output ()
    (progn
      (setf (lift::test-result-property *test-result* :style-sheet) "test-style.css")
      (setf (lift::test-result-property *test-result* :title) "nisp")
      (setf (lift::test-result-property *test-result* :if-exists) :supersede)
      (lift::test-result-report *test-result* #p "/home/james/lisp/nisp/lift-tests/html/report" :html)
      (setf (lift::test-result-property *test-result* :style-sheet) "test-style.css")
      (setf (lift::test-result-property *test-result* :title) "nisp")
      (setf (lift::test-result-property *test-result* :if-exists) :supersede)
      
      (lift::test-result-report *test-result* (concatenate 'string "/home/james/lisp/nisp/lift-tests/save/report-" (lift::date-stamp :include-time? t) ".sav") :save)
      (trivial-shell::shell-command "scp /home/james/lisp/nisp/lift-tests/html/* vps:paste/nisp/")
      (trivial-shell::shell-command "scp /home/james/lisp/nisp/lift-tests/save/* vps:paste/nisp/save/")))

(defun generate-tinaa-docs ()
  (tinaa:document-system
   'package :nispbot "/home/james/lisp/nisp/tinaa/")
   (trivial-shell::shell-command "scp -r /home/james/lisp/nisp/tinaa/* vps:paste/nisp/tinaa/")
  )