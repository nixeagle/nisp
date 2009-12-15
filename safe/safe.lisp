;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(defpackage #:nisp-safe
  (:use :common-lisp
        :lift
        :metatilities
        :nisp-empty-package
        :nisp-util)
  (:export #:with-safe-package
           #:with-safe-readtable
           :safe
           #:safe-read
           #:safe-select
           #:make-safe
           ))

(defpackage #:safe-testing!
  (:use)
  (:shadowing-import-from :cl #:loop #:mapc #:mapcar #:list #:lambda :t :nil)
  (:export
           #:loop
           #:mapc
           #:mapcar
           #:list
           #:lambda
           #:describe
           :t
           :nil))

(defpackage #:safe-external
  (:use)
;  (:shadow help)
  (:export #:range help))

(defpackage #:safe-closure
  (:use)
  (:export #:reset #:setq :nisp-test))

(in-package :nisp-safe)

(defparameter *prepared-safe-packages*
  '(:safe-arithmetic
    :safe-arithmetic-trig
    :safe-arithmetic-comparision
    :safe-arithmetic-type-manipulation
    :safe-arithmetic-boole
    :safe-arithmetic-implentation-constants
    :safe-arithmetic-random
    :nisp-safe-introspect
    :safe-external
    :safe-closure
    :safe-testing!)
  "Listing of packages that have been prepared or deemed to be safe.
This is the default list, the idea is depending on the situation mix and
match what capacities you want to allow untrusted code to do.

On the todo list is to create a class that allows multiple instances and
tracks currently interned packages and whatnot.")

(defun make-safe-package (name)
  "Should make a totally independent package. All normal safe stuff should be in this by default.

Some questions to consider:
  - Is it safe to set *readtable* in the package on creation. By this is it possible for a malicious user to modify this?
    - What happens when I set it to nil?
    - Can I replace colon-reader with another more permissive function? If so how?
  - Is it safe to read using a read function defined in this package? My off the cuff guess is 'not a good idea'. Too much chance for iffy behavior unless it can be proven safe."
  (with-package (make-empty-package name)
    (cl::use-package nisp-safe::*prepared-safe-packages*)
    *package*))

(defun delete-safe-package-old (name)
  "Delete package NAME unless its already deleted."
  (when (packagep (find-package name))
    (delete-package name)))

(defun read-using-package (name string)
  "read STRING using package NAME."
  (let ((*package* (find-package name)))
    (read-from-string string)))

(defun colon-reader (stream char)
  "Signal an error if a colon is used to access another package."
  (declare (ignore stream char))
  ;; For now we just error out. This is a safe thing to do as far as
  ;; disabling package access goes, but in the process use of : is
  ;; broken for :keywords and possibly other things too.
  (error "Accessing packages outside of the current one is disabled."))

(defun make-readtable ()
  "Create readtable that prevents any syntax that can cause a package
  change."
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\: #'colon-reader nil *readtable*)
    *readtable*))

(defmacro with-safe-readtable (&body body)
  "Use readtable for all read calls in body.If readtable is not passed,
we default to instantiating a new one using make-readtable"
  `(let ((*readtable* *safe-readtable*))
     ,@body))

;;; Moving this down here for now after make-readtable is defined
;;; because this depends on that function being defined.
(defparameter *safe-readtable* (make-readtable)
  "The safe readtable that is used by the with-safe-readtable macro.

This is currently a parameter but should be made into a constant at some
point so a warning or error can block it being changed in a running
program.")

(defmacro with-safe-package (name &body body)
  "Use the given package name to read some code."
  `(with-package ,name
     (with-safe-readtable
       ,@body)))


(defclass safe ()
  ((packages :accessor safe-packages
             :initform (make-hash-table :test 'equal)))
  (:documentation "Primary class for groupings of safe packages and manipulation of these packages."))

(defgeneric safe-select (safe owner))
(defmethod safe-select ((safe safe) (owner string))
  "Select a package, if the package does not exist create one and return it."
  (or (gethash owner (safe-packages safe))
      (setf (gethash owner (safe-packages safe))
            (create-safe-package (concatenate 'string "SAFE-" owner)
                                 owner))))
(defgeneric safe-delete-package-list (safe)
  (:documentation "Delete the list of packages known to a safe object."))
(defmethod safe-delete-package-list ((safe safe))
  (maphash (lambda (key package)
             (declare (ignore key))
             (delete-safe-package package))
           (safe-packages safe))
  (clrhash (safe-packages safe)))

(defun make-safe ()
  (make-instance 'safe))

(defclass safe-package ()
  ((package :accessor safe-package
            :initarg :package)
   (creation-time :initform (get-universal-time)
                  :accessor package-creation-time)
   (readtable :accessor safe-package-readtable
              :initform (make-readtable))
   (owner :accessor safe-package-owner
          :initarg :owner)
   (use :accessor safe-package-use
        :initform *prepared-safe-packages*)))

(defgeneric create-safe-package (package &optional owner))
(defmethod create-safe-package ((package safe-package) &optional owner)
  (declare (ignore owner))
  (setf (safe-package package)
        (make-empty-package (concatenate 'string "SAFE-"
                                    (safe-package-owner package))))
   (add-package package (safe-package-use package)))
(defmethod create-safe-package ((package package) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package package
                             :owner owner)))
    (add-package safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))
(defmethod create-safe-package ((package string) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package (make-empty-package package)
                             :owner owner)))
    (add-package safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))
#+ (or)
(defmethod create-safe-package :after ((package string) &optional owner)
  (declare (ignore owner))
  (add-package  (find-package package) *prepared-safe-packages*))
#+ (or)
(defmethod create-safe-package :after ((package package) &optional owner)
  (declare (ignore owner))
  (add-package package *prepared-safe-packages*))

(defgeneric add-package (safe-package package-name)
  (:documentation "add symbols from another package"))
(defmethod add-package ((package safe-package) package-name)
  (with-safe-package (safe-package package)
    (use-package package-name (safe-package package))))

(defgeneric remove-package (safe-package package-name)
  (:documentation "remove symbols from another package"))

(defgeneric clear-safe-package (safe-package))

(defgeneric delete-safe-package (safe-package))
(defmethod delete-safe-package ((package safe-package))
  (delete-package (package-name (safe-package package))))
(defmethod delete-safe-package ((name string))
  (delete-package name))

(defgeneric safe-read (package forms &optional owner)
  (:documentation "(read input using the namespace of the safe package"))

(defmethod safe-read ((package safe-package) (forms string) &optional owner)
  (declare (ignore owner))
  (with-package (package-name (safe-package package))
    (let ((*readtable* (safe-package-readtable package)))
      (multiple-value-bind (result)
          (read-from-string forms)
        result))))

(defmethod safe-read ((safe safe) (forms string) &optional owner)
  (safe-read (safe-select safe owner) forms))

(defun safe-external::describe (object)
  "Special describe function for our sandbox testing stuff."
  ;; Needs rewritten to not depend on swank!
  (swank::describe-to-string object))

(defun safe-external::range (start end)
  "Generate a list of integers from start to end."
  (if (> start end)
      (loop for x from start downto end collect x)
      (loop for x from start to end collect x)))

(defparameter safe-external::help "Welcome to nisp-safe! This is a tool for evaluating common lisp in a safe environment. Currently you can set your own variables with (setq quux \"bar\") and reset your 'sandbox' with (reset). Each user gets their own area."
  "The help message when a user types the word help in.")

(defgeneric build-symbol-name (package symbol))

(defmethod build-symbol-name ((safe-package safe-package) (symbol symbol))
  (intern (concatenate 'string
           (package-name (safe-package safe-package))
           "::"
           (symbol-name symbol))))

(defgeneric safe-package-intern (package object)
  (:documentation "intern a new copy of object and setting that copy to the value of the other package's object."))

(defmethod safe-package-intern ((safe-package safe-package) (symbol symbol))
  (shadow symbol (safe-package safe-package))

  (let ((new-symbol
         (intern (concatenate 'string (symbol-name symbol))
                 (safe-package safe-package))))
         
    new-symbol))
;; (defmacro define-variable-nisp (string value &optional (documentation "None given"))
;;   `(defvar ,string ,value ,documentation))

(let ((closed-list))
  (defun list-to-pair (&optional new-list)
    "Convert list to pairs, givin list only set the list, not return a pair."
    (if new-list
        (setq closed-list new-list)
        (when closed-list
          (unless (evenp (list-length closed-list))
            (error "List has odd number of elements."))
          (values (pop closed-list) (pop closed-list))))))

(defun list-to-pairs (list)
  (list-to-pair list)
  (loop as pair = (multiple-value-list (list-to-pair))
     while (= (list-length pair) 2)
       collect pair))

(defmacro setq-single (var value)
  (list 'cl:setq var value))


(defun nisp-safe::populate-safe-package-closures (safe-package)
  "Reset the functions that have some hidden state."
  (defun safe-closure::reset ()
    "Reset the sandbox you are in. Generally this will delete the sandbox and create a new one in its place."
    (delete-safe-package safe-package)
    (create-safe-package safe-package))
  (defmacro safe-closure::setq (&rest things)
    "Special macro defined to wrap around the base setq given by the
lisp implentation. The primary thing we do in this macro is be sure to intern new symbols in the safe-package and not allow modification of symbols exported from other packages." 
    `(mapcar #'safe-closure::setq-pair
             ',(list-to-pairs things)))
  
  (defun safe-closure::setq-pair (pair)
    (let ((f (safe-closure::safe-intern (first pair)))
          (l (second pair)))
      (safe-closure::setq-trace2 f l)))

  ;; THIS IS SO WRONG! But it works. Find out why and do this right!
  (defun safe-closure::setq-trace2 (first2 last2)
    (eval `(setq ,first2 ,last2)))
  
  (defun safe-closure::safe-intern (thing)
    "Given a thing, intern it if its something that can cause package classes otherwise just leave it alone."
    (if (typep thing 'symbol)
        (safe-package-intern safe-package thing)
        thing))
  
  (defun safe-closure::nisp-test (&rest things)
    (let ((*package* (safe-package safe-package)))
      (prin1-to-string (macroexpand-1 `(safe-closure::setq ,things)))
      #+ ()      (mapcar #'safe-closure::safe-intern things))
    #+ ()   (mapcar #'class-of things)))

