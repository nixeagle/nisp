;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(in-package :nisp-safe)



(defun delete-safe-package-old (name)
  "Delete package NAME unless its already deleted.

DEPRECIATED
Moving to all safe-package operations being on safe-package and or safe."
  (when (packagep (find-package name))
    (delete-package name)))

(defun read-using-package (name string)
  "read STRING using package NAME."
  (let ((*package* (find-package name)))
    (read-from-string string)))


(defclass safe-set ()
  ((packages :accessor safe-set
             :initform (make-hash-table :test 'equal)))
  (:documentation
   "Primary class for groupings of safe packages and manipulation of
these packages. These are used to group related packages together and
perform operations on sets of packages."))

(defgeneric safe-select (safe owner))
(defmethod safe-select ((safe safe-set) (owner string))
  "Select a package, if the package does not exist create one and return it."
  (or (gethash owner (safe-set safe))
      (setf (gethash owner (safe-set safe))
            (create-safe-package (concatenate 'string "SAFE-" owner)
                                 owner))))
(defgeneric safe-delete-package-list (safe)
  (:documentation "Delete the list of packages known to a safe object."))
(defmethod safe-delete-package-list ((safe safe-set))
  (maphash (lambda (key package)
             (declare (ignore key))
             (delete-safe-package package))
           (safe-set safe))
  (clrhash (safe-set safe)))

(defun make-safe-set ()
  "Make a new instance of a safe package set."
  (make-instance 'safe-set))

(defgeneric safe-read (package forms &optional owner)
  (:documentation "(read input using the namespace of the safe package"))

(defmethod safe-read ((package safe-package) (forms string) &optional owner)
  (declare (ignore owner))
  (with-package (package-name (safe-package package))
    (let ((*readtable* (safe-package-readtable package))
          (*read-eval* nil))            ;disable #.foo
      (multiple-value-bind (result)
          (read-from-string forms)
        result))))

(defmethod safe-read ((safe safe-set) (forms string) &optional owner)
  (safe-read (safe-select safe owner) forms))

(defun safe-external::describe (object)
  "Special describe function for our sandbox testing stuff."
  ;; Needs rewritten to not depend on swank!
  (swank::describe-to-string object))

(defparameter safe-external::help "Welcome to nisp-safe! This is a tool for evaluating common lisp in a safe environment. Currently you can reset your 'sandbox' with (reset). Each user gets their own area."
  "The help message when a user types the word help in.")

(defparameter safe-external::test-results "http://paste.nixeagle.org/lift-nisp/"
  "Location of the latest test run results.")

(defmacro setq-single (var value)
  (list 'cl:setq var value))

;;; Also from metatilities
(defun group (list n)
  "Return the elements of `list' grouped in sublists of length `n,'
which should be a positive integer.  For example, the list '(a b c d)
grouped by 2 yields '((a b) (c d)).  Definition of `group' taken from On
Lisp, by Paul Graham.  The definition is not hugely efficient, so this
function is appropriate for parsing macro args, but no for inner loops.

Example:
 (group (range 1 10) 2)
=> ((1 2) (3 4) (5 6) (7 8) (9 10))

 (group (range 1 10) 5)
=> ((1 2 3 4 5) (6 7 8 9 10))

 (group (range 1 10) 7)
=> ((1 2 3 4 5 6 7) (8 9 10))
"
  (check-type list list)
  (check-type n (integer 1 #.most-positive-fixnum))
  (labels ((next (list n acc)
	     (let ((rest (nthcdr n list)))
	       (if (consp rest)
		   (next rest n (cons (subseq list 0 n) acc))
		   (nreverse (cons list acc))))))
    (next list n nil)))

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
             ',(group things 2)))  ;;; metautilities group being used here.
  
  (defun safe-closure::setq-pair (pair)
    (let ((f (shadowing-intern safe-package (first pair)))
          (l (second pair)))
      (setq f (first pair))     ;Set a copy... leaving f interned right
      (safe-closure::setq-trace2 f l)))

  ;; THIS IS SO WRONG! But it works. Find out why and do this right!
  (defun safe-closure::setq-trace2 (first2 last2)
    (eval `(setq ,first2 ,last2)))
  
  (defun safe-closure::safe-intern (thing)
    "Given a thing, intern it if its something that can cause package classes otherwise just leave it alone."
    (if (typep thing 'symbol)
        (shadowing-intern safe-package thing)
        thing))
  
  (defun safe-closure::nisp-test (&rest things)
    (let ((*package* (safe-package safe-package)))
      (prin1-to-string (macroexpand-1 `(safe-closure::setq ,things)))
      #+ ()      (mapcar #'safe-closure::safe-intern things))
    #+ ()   (mapcar #'class-of things)))