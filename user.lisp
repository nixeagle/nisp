;;;; Package for including all sorts of interesting and useful libraries
;;;; for testing.
(in-package :cl-user)

(defpackage #:nisp.user
  (:use :cl
        ;; implentation independent sockets
        :alexandria
        :anaphora
        :eos
        :usocket
        :drakma)                         ;http-request
  (:nicknames :nu :~))

(in-package :nisp.user)
;;; In custom userland, this userland has modifications from a standard
;;; userland.

(defmacro n.disasemble (form)
  (let ((s (gensym)))
    `(let ((,s (with-output-to-string (*standard-output*)
                 (disassemble ',(car form)))))
       (print ,s)
       (values
        ,form
        (count #\Newline ,s)))))
(defun ensure-no-leading-single-quote (string)
  "Append a leading ' to STRING if there is not already one."
  (declare (type string string))
  (if (char= (char string 0) #\')
      (subseq string 1)
      string))

(define-condition nisp-user-error (error)
  ())

(define-condition package-not-found-error (nisp-user-error)
  ((package :initarg :package :reader package-not-found-error-package))
  (:report (lambda (condition stream)
             (format stream "Package ~A is not loaded"
                     (package-not-found-error-package condition)))))

(define-condition swank-package-not-found-error (package-not-found-error)
  ()
  (:default-initargs :package :swank))

(defgeneric eval-in-emacs (form &optional nowait))
(defmethod eval-in-emacs ((form string) &optional nowait)
  (eval-in-emacs (read-from-string (ensure-no-leading-single-quote form))
                 nowait))
(defmethod eval-in-emacs (form &optional nowait)
  (let ((swank::*emacs-connection* (swank::default-connection)))
    (swank::eval-in-emacs form nowait)))

(defun my-setup-server (&optional (port 2288))
  "Load up a swank instance ready to be connected to by emacs."
  (swank:create-server :dont-close t :coding-system "utf-8-unix" :port port)
  (setf swank:*use-dedicated-output-stream* nil))


(defmacro clock (form &optional (iterations 100000) (_count_ 0))
  "Execute FORM ITERATIONS times.

The result is an alist of the form:
  ((time . rational) (iterations . integer) (result . <form result>))"
  ;; This macro has a bug, it evaluates FORM one extra time at the end
  ;; of the macro expression to display the result value.
  (declare (type positive-fixnum)
           (optimize (debug 0) (speed 3) (safety 0)
                     (space 1) (compilation-speed 3)))
  (let ((start (gensym))
        (total (gensym))
        (avg (gensym)))
    `(iter (with ,start = (get-internal-real-time))
            (for _count_ :from 0 :to ,iterations)
            ,form
            (finally (let* ((,total (the (or (integer 0 0) positive-fixnum)
                                      (- (get-internal-real-time) ,start)))
                            (,avg (/ ,total
                                     (* ,iterations
                                        internal-time-units-per-second))))
                       (return
                         (list
                          (cons :time (list
                                       (/ ,total internal-time-units-per-second)
                                       (format nil "~f"
                                               (/ ,total
                                                  internal-time-units-per-second))))
                          (cons :avg (list ,avg (format nil "~f" ,avg)))
                          (cons :result ,form))))))))

(defparameter *call-times* ())
(defvar *call-times-lock* (bt:make-lock))
(defun call-blocktimer (thunk)
  (let ((start-time (get-internal-real-time)))
    (prog1
        (funcall thunk)
      (let ((total-time
             (/ (- (get-internal-real-time) start-time)
                internal-time-units-per-second)))
        (bt:with-lock-held (*call-times-lock*)
          (push total-time
           *call-times*))))))

(defun call-blocktimer-return-time (thunk)
  (let ((start-time (get-internal-real-time)))
    (progn
        (funcall thunk)
      (/ (- (get-internal-real-time) start-time)
         internal-time-units-per-second))))

(defmacro blocktimer (&body body)
  `(call-blocktimer (lambda () ,@body)))

(defmacro qtime (iterations &body body)
  "Time BODY over ITERATIONS loops."
  `(values
    (float (call-blocktimer-return-time
      (lambda () (dotimes (i ,(expt 2 iterations))
                   ,@body))))
    ,(expt 2 iterations)))
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

(in-package :nisp.user)


(in-package :swank)
(defslimefun documentation-symbol (symbol-name)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (with-output-to-string (string)
              (format string "Documentation for the symbol ~a:~2%" sym)
              (unless (or vdoc fdoc)
                (format string "Not documented." ))
              (when vdoc
                (format string "Variable:~%   Value: ~A~%~%~a~2%"  (with-output-to-string (*standard-output*) (let ((*print-right-margin* 70))
                                                                                                                (pprint (symbol-value sym)))) vdoc))
              (when fdoc
                (format string "Function:~% Arglist: ~a~2% ~a"
                        (swank-backend:arglist sym)
                        fdoc))))
          (format nil "No such symbol, ~a." symbol-name)))))


(define-method-combination hook (&optional (function 'progn))
  ((methods *))
  "Call functions like they would be called on an emacs hook.

As with emacs hooks the order that methods are called in should not be
depended upon."
  `(,function ,@(mapcar (lambda (method)
                      `(call-method ,method))
                    methods)))

(define-method-combination random-elt ()
  ((methods *))
  `(progn
     ,@(mapcar (lambda (method)
                `(call-method ,method))
               methods)))

(defclass random-elt-generic-function
    (cl:standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod compute-discriminating-function ((gf random-elt-generic-function))
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (funcall (method-function
              (random-elt (compute-applicable-methods gf args)))
             args nil)))