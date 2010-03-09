(require :asdf)
(require :sb-posix)
(pushnew "/usr/share/common-lisp/systems/" asdf:*central-registry*)

(asdf:load-system :asdf-install)

(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf asdf-install))



;(save-lisp-and-die "/home/james/sbcl.core-for-slime")
;(save-lisp-and-die "/home/nixeagle/lisp/sbcl.core-for-slime")




;(declaim (optimize sb-cover:store-coverage-data))

;(load "/home/james/lisp/scheme/r6rs.asd")


#+sbcl
(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

;;; If I have 5am and sbcl running I want to ignore redefinition warnings
;;; due to a bug with how it handles redefining tests.
#+ (and sbcl 5am)
  (setq sb-ext:*muffled-warnings* 'sb-kernel::redefinition-warning)

#+sbcl
(progn
  (setq sb-ext:*efficiency-note-cost-threshold* 2)
  (setq sb-ext:*derive-function-types* t))
#+ ()
(handler-bind ((asdf-install::key-not-found (lambda (x) (let ((restart (find-restart 'asdf-install::skip-gpg-check)))     (when restart (invoke-restart restart)))))) (asdf-install:install :metatilities))

(pushnew "/home/james/lisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/emacs/slime/" asdf:*central-registry*)
(pushnew "/home/james/lisp/nisp/emacs/slime/contrib/" asdf:*central-registry*)

(require :swank)
(require :eos)

#+:sbcl
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)
                   (compilation-speed 0)))

(setq
 swank:*inspector-verbose* t
 swank:*globally-redirect-io* nil)

(defun my-setup-server (&optional (port 2288))
  "Load up a swank instance ready to be connected to by emacs."
  (swank:create-server :dont-close t :coding-system "utf-8-unix" :port port)
  (setf swank:*use-dedicated-output-stream* nil))


;#+sbcl
;(my-setup-server)

(defpackage #:nix-emacs
  (:use :cl :swank))

(in-package :nix-emacs)

(defun nix-pprint (values)
  (swank::with-buffer-syntax ()

    (let ((*PRINT-CIRCLE* nil)
          (*PRINT-PRETTY*  t)
          (*PRINT-ESCAPE*  t)
          (*PRINT-ARRAY* t)
          (*PRINT-LINES* nil)
          (*PRINT-LEVEL* nil)
          (*PRINT-LENGTH* nil)
          (*PRINT-RIGHT-MARGIN* 72)
          (*print-readably* nil))
      (with-output-to-string (*standard-output*)
        (dolist (o values)
          (pprint o))))))
(defun describe-values (object)
  (mapcar
   (lambda (x)
     (with-output-to-string (*standard-output*)
       (describe x)))
   (if (and (listp object) (listp (car object)))
       (car object)
       object)))
(defun nix-pprint-eval (string &optional extra)
  (declare (ignore extra))
  (swank::with-buffer-syntax ()
    (let* ((*PRINT-CIRCLE* nil)
           (*PRINT-PRETTY*  t)
           (*PRINT-ESCAPE*  t)
           (*PRINT-ARRAY* t)
           (eos:*run-test-when-defined* t)
           (*PRINT-LINES* nil)
           (*PRINT-LEVEL* nil)
           (*PRINT-LENGTH* nil)
           (*print-right-margin* 78)
           (*print-readably* nil)
           (stand (make-string-output-stream))
           (trace (make-string-output-stream))
           (err (make-string-output-stream))
           (*standard-output* stand)
           (*trace-output* trace)
           (*error-output* err)
           (form (read-from-string string))
           (values (multiple-value-list
                    (eval form))))
      (list string
            (get-output-stream-string stand)
            (get-output-stream-string err)
            (get-output-stream-string trace)
            (nix-pprint values)
            (describe-values values)))))

(in-package :cl-user)
;#+sbcl
;(asdf:load-system :nispbot)

;#+sbcl
;(in-package :nispbot)

;(in-package :cl-user)

(asdf:operate 'asdf:load-op :nisp.user)

(in-package :nisp.user)