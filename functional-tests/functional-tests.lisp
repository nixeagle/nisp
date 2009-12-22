;;;test framework idea: embed the test and expected results in the
;;;docstring itself in addition to more specialized tests by
;;;function. Even better is defining the tests seperately and modifying
;;;the function docstring to append examples and such. Thus the test
;;;becomes part of the function's own documentation. See
;;;(metatilities:depreciated) and (metatilities:make-obsolete) for how
;;;to do this. That way the tests become the documentation.

;;;It is very important that we do not get outside of testing *one*
;;;function at a time and gearing the tests to that. Doing them any
;;;other way just adds extra overhead that really does not need to
;;;exist. I don't want to type the function's name repeatedly, and I
;;;want the testcases to show up with (describe) for online
;;;help. Examples work very well with instructing others how a system is
;;;intended to be used.

;;; I am not 100% positive how functions with side effects will be
;;; tested other then attempting to isolate the sideeffects, maybe by
;;; using some of the work being done in :nisp-safe to allow a function
;;; to modify its own package without touching stuff otuside of it.

;;; Right from the start I want to indicate that heirarchical tests are
;;; not an intended feature outside of not running tests that use a
;;; function with a failed test. But that is not doing explicent nested
;;; test suites as done by those that follow the *Unit philosopy. Please
;;; understand I've tried 3 different frameworks extensively and none of
;;; them really fit the way lisp is designed. We don't have objects that
;;; *contain* methods. We have functions that act on objects. Test the
;;; *functions* and the object is tested as well.
;;;
;;; Tests will be placed into the same package as it is defined by
;;; default as the function it is testing. Alternatively an option will
;;; be given to put the tests in a seperate package defined by the
;;; application programmer. Running the tests is as easy as picking one
;;; test and running it, or running all tests in a single
;;; package. Grouping beyond that is to be left to the application
;;; programmer. Tools like mapc and others exist for this reason. I
;;; don't want to take too much control over the tests away from the
;;; programmer.

;;Require at minimum an object to hold the function being tested, This
;;can contain function wide profiling information (profile no profile),
;;test/pass results for the group of IOsets on the function as well as
;;what functions should be traced if tracing is turned on during the
;;test execution. We can determine dependency information somewhat from
;;the trace list, but be sure to include a way to mention a function
;;dependency without tracing.

;;an object that contains an IO set, which is one input matched to
;;expected error, output or value. Something like: (or output (xor error
;;value)) are valid combinations. The IO set could also log the results
;;for each call with that input as well as store if that set should be
;;traced or not on the input/output level. Of course use a few macros to
;;abstract msot of those ugly details away. A test should look something
;;like this:

;;(define-test function-name :profile t :trace nil
;; "some documentation if wanted, else default to function docs"

(defpackage #:functional-tests
  (:use :cl))

(in-package :functional-tests)

;;; Probably not going to get used at this point. We don't want to have
;;; ONE list that everyone has to share
(defparameter *functional-tests-list* ()
  "Temporary global special for holding function-test objects until this
is bootstrapped some more.")

;;; Don't use this for anything with heavy computation, we can do it
;;; here as this is a test framework, not a load heavy appliction
(deftype fbound ()
  "An fbound symbol."
  '(and symbol
    (satisfies fboundp)))

(defclass function-test ()
  ((fbound-object :initarg :fbound
                  :accessor test-fbound
                  :documentation "Any object that can be funcalled")
   (io-sets :initarg :sets
            :accessor test-sets
            :initform ())))

(defclass io-expected-result ()
  ((output :initarg :output
           :accessor result-output
           :initform ""
           :type (or string nil)
           :documentation "Output as if to *standard-output*.")
   (value :initarg :value
          :accessor result-value
          :documentation "Any lisp form that produces the expected value.")
   (condition :initarg :signal
              :accessor result-condition)))

(defclass io-actual-result (io-expected-result)
  ((creation-time :initform (get-universal-time)
                  :accessor result-creation-time)
   (run-time :initform nil
             :accessor result-run-time
             :initarg :run-time)
   (trace :initarg :trace
          :accessor result-trace)))

;;; A set is a result with included input to get the result.
;;; We also log results on a set level. Of course results are io-results
;;; as the input never changes.
;;;
;;; Please note that a set is not associated with any particular
;;; function by default. It is just an abstract "in -> out" construct
;;; that tries to maintain a history of past in -> out results
(defclass io-set (io-expected-result)
  ((name :initarg :name
         :accessor io-set-name
         :type string)         ;Needed?
   (input :initarg :input
          :accessor io-set-input
          :type list
          :documentation "Lambda list as it would be passed into the
 real function.")
   (result-log :initform ()
               :accessor io-set-result-log)))

(defun make-io-expected-result (value &key signal output)
  (make-instance 'io-expected-result
                 :value value
                 :signal signal
                 :output output))

(defun make-io-set (input value &key signal output)
  (make-instance 'io-set
                 :input input
                 :value value
                 :signal signal
                 :output output))


(defun make-function-test (function &rest io-sets)
  (make-instance 'function-test :fbound function
                 :sets io-sets))

(defgeneric run-test-set (test input)
  (:documentation "Run a test set and get results"))
(defmethod run-test-set ((test function) (input io-set))
  "The easiest case, we get as input the objects we want to run a single test set"
  (apply test (io-set-input input)))
(defmethod run-test-set ((test function-test) (set io-set))
  (run-test-set (test-fbound test) set))
(defmethod run-test-set ((test function-test) (set (eql :all)))
  (mapcar (lambda (x) (run-test-set test x))
          (test-sets test)))

(defmacro define-function-test (function-name)
  "Very basic, will be rewritten."
  `(push (make-function-test #',function-name) *functional-tests-list*))

(defgeneric add-test-set (fbound input value &optional output)
  (:documentation "Add another set of input->values"))
(defmethod add-test-set ((test function-test) input value &optional output)
  (setf (test-sets test)
        (list (make-io-set input value :output output))))

#+nil
(defgeneric add-test-to-plist (fbound input value)
  (:documentation "Add a test case to a function's plist"))

(defun fbound-plist-tests-p (fbound)
  "Return t if FBOUND has a plist with tests."
  ;; look up what a macro is as far as typing
;  (declare (type symbol fbound))
  (declare (type fbound fbound))
  (not (not (get fbound :ftests))))


(defun add-test-to-plist (fbound input result)
  "Add a test with FBOUND using INPUT expecting RESULT."
  (declare (type (or symbol) fbound)
           (type (or list) input))
  (set-fbound-plist-tests fbound (make-io-set input result)))

(defun get-fbound-plist-tests (fbound)
  (declare (type (or symbol) fbound))
  (the list (get fbound :ftests)))

(defun set-fbound-plist-tests (fbound &rest io-sets)
  "Destructively replace the old plist tests with IO-SETS"
  (declare (type symbol fbound))
  (check-type fbound (satisfies fboundp))
  (the list (setf (get fbound :ftests) io-sets)))

(defgeneric map-fbound-plist-tests (fbound)
  (:documentation "Run all tests in the list"))
(defmethod map-fbound-plist-tests ((fbound symbol))
  "On a symbol run the symbol's plist"
  (check-type fbound (satisfies fboundp))
  (check-type (getf (symbol-plist fbound) :ftest) list)
  (mapcar (lambda (test)
            (declare (type io-set test))
            (run-test-set (symbol-function fbound) test))
          (get-fbound-plist-tests fbound)))

;(equalp (make-function-test #'+) (make-function-test #'+))




