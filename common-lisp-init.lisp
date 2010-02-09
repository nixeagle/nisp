;(require :asdf)
(pushnew "/usr/share/common-lisp/systems/" asdf:*central-registry*)

(pushnew "/home/james/lisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/asdf/" asdf:*central-registry* :test #'equal)

;;; debian ecl asdf install does not work right.
;;; it has a problem with trying to load sbcl specific things.
#+ (or ecl clisp)
(pushnew "/home/james/lisp/asdf-install/asdf-install/" asdf:*central-registry*)
#+:ecl
;ecl
(load "/home/james/lisp/asdf-install/asdf-install/load-asdf-install.lisp")

#+:clisp
(pushnew "/home/james/.asdf-install-dir/systems/" asdf:*central-registry*)
;;; gcl is cltl1, so asdf-install probably won't work
;;; ecl seems to not have asdf preloaded yet.
#+ (not (or gcl ecl))
(asdf:load-system :asdf-install)

;(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf asdf-install))

;(save-lisp-and-die "/home/james/sbcl.core-for-slime")
;(save-lisp-and-die "/home/nixeagle/lisp/sbcl.core-for-slime")




;(declaim (optimize sb-cover:store-coverage-data))

;(load "/home/james/lisp/scheme/r6rs.asd")


#+sbcl
(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
#+:sbcl
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

;;; If I have 5am and sbcl running I want to ignore redefinition warnings
;;; due to a bug with how it handles redefining tests.
#+ (and sbcl 5am)
  (setq sb-ext:*muffled-warnings* 'sb-kernel::redefinition-warning)

#+sbcl
(progn
  (setq sb-ext:*efficiency-note-cost-threshold* 2)
  (setq sb-ext:*derive-function-types* t))

(require :swank)


(setq       
 swank:*inspector-verbose* t
 swank:*globally-redirect-io* t)

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
    
    (let ((*PRINT-CIRCLE* t) 
          (*PRINT-PRETTY*  t) 
          (*PRINT-ESCAPE*  t)
          (*PRINT-ARRAY* t)
          (*PRINT-LINES* nil)
          (*PRINT-LEVEL* nil) 
          (*PRINT-LENGTH* nil)
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
           (*PRINT-LINES* nil)
           (*PRINT-LEVEL* nil) 
           (*PRINT-LENGTH* nil)
           (*print-right-margin* (1- (expt 2 26)))
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