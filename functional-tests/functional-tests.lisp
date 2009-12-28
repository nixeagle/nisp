(defpackage #:functional-tests
  (:use :cl))

(in-package :functional-tests)

(defparameter +plist-keyword+ :functional-tests
  "Inserting tests in")


;;; Don't use this for anything with heavy computation, we can do it
;;; here as this is a test framework, not a load heavy appliction
(deftype fbound ()
  "An fbound symbol."
  '(and symbol
    (satisfies fboundp)))

(deftype possible-package ()
  "A package or symbol that can be a package"
  '(or symbol package))

;;; Definition taken from reading on lisp
(defun single (1ist)
  "Return t if input is a list of one element.

Example:
 (single 5)
=> NIL

 (single t)
=> NIL

 (single (list t))
=> T

 (single (list \"Something\"))
=> T
"
  (and (consp 1ist) (not (last 1ist))))

(defclass io-expected-result ()
  ;; Should this get changed to io-expected or does that lose clarity as
  ;; far as what is going on?
  ((output :initarg :output
           :accessor result-output
           :initform ""
           :type string
           :documentation "Output as if to *standard-output*.")
   (value :initarg :value
          :accessor result-value
          :documentation "Any lisp form that produces the expected value.")
   (condition :initarg :signal
              :accessor result-condition)))

(defclass io-result (io-expected-result)
  ((creation-time :initform (get-universal-time)
                  ;; No real need to modify creation time
                  :reader result-creation-time
                  :documentation "Time that the io-result is created.")
   (end-time :initform 0
             :accessor result-end-time
             :documentation "Test completion time.")
   (trace :initarg :trace
          :accessor result-trace)))

(defclass io-log ()
  ((results :initform ()
            :accessor io-log-results
            :initarg :results
            :type list))
  (:documentation "Keeps track of an io-set's test results."))

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
         :initform ""
         :type string)         ;Needed?
   (input :initarg :input
          :accessor io-set-input
          :type list
          :documentation "Lambda list as it would be passed into the
 real function.")
   (log :initform (make-instance 'io-log)
        :accessor io-set-log
        :type io-log)))

(defun make-io-expected-result (value &key signal output)
  (make-instance 'io-expected-result
                 :value value
                 :signal signal
                 :output output))

(defun make-io-set (input value &key signal output)
  (make-instance 'io-set
                 :input input
                 :value value
                 :signal (or signal "")
                 :output (or output "")))

;;;; Working with the actual results
(defun make-io-result ()
  "Brand new expected result.

From the moment of creation until the test ends consists of the total
runtime of this set of results."
  (make-instance 'io-result))

;;;; Getting and setting plists ----------------------------------------
(declaim (ftype (function (fbound) (values list &optional))
                get-fbound-plist-tests)
         (ftype (function (fbound &rest io-set)
                          (values list &optional))
                set-fbound-plist-tests)
         (ftype (function (fbound)
                          (values (member nil) &optional))
                clear-fbound-plist-tests))

(defun get-fbound-plist-tests (fbound)
  "Get the list of tests"
  (get fbound +plist-keyword+))

(defun set-fbound-plist-tests (fbound &rest io-sets)
  "Destructively replace the old plist tests with IO-SETS"
  (setf (get fbound +plist-keyword+) io-sets))

(defun clear-fbound-plist-tests (fbound)
  "Remove all tests on FBOUND"
  (set-fbound-plist-tests fbound))

;;;; Equality ----------------------------------------------------------
(declaim (ftype (function (io-set (or io-set list))
                          (values boolean &optional))
                io-set-equalp))

(defun io-set-equalp (x y)
  "Two io-sets test for the same thing if their input is the same."
  ;; We do not really care about the other values as we presume this
  ;; test is being done for io-sets operating on the same function. If
  ;; this assumption is false, the test will give invalid results.
  (equal (io-set-input x)
         (io-set-input (if (listp y) (car y) y))))

;;;; Adding and removing tests -----------------------------------------
;;; Given any test related object, add or remove a test from that
;;; object's test list

(defgeneric remove-fbound-plist-test (fbound test)
  (:documentation "Remove one TEST from FBOUND's list.")
  (:method ((fbound symbol) (test io-set))
      (apply #'set-fbound-plist-tests fbound
       (remove test (get-fbound-plist-tests fbound)
               :test #'io-set-equalp))))

(defgeneric add-fbound-plist-test (fbound test &optional result)
  (:documentation "Append a new test to FBOUND's list.")
  (:method ((fbound symbol) (test io-set) &optional result)
           "Append io-set TEST to FBOUND."
           (declare (ignore result))
           (apply #'set-fbound-plist-tests
                  fbound (adjoin test (get-fbound-plist-tests fbound)
                    :test #'io-set-equalp))))


(declaim (ftype (function (fbound io-set)
                          (values boolean &optional))
                fbound-plist-test-p)
         (ftype (function (fbound)
                          (values boolean &optional))
                fbound-plist-tests-p))

;;;; "Type" checking ---------------------------------------------------
;;; Check if an object has a test.

(defun fbound-plist-tests-p (fbound)
  "Return t if FBOUND has a plist with tests."
  ;; Absolutely needs a better name, this is just plural of another
  ;; function name: bad bad!
  (not (not (get fbound +plist-keyword+))))

(defun fbound-plist-test-p (fbound test)
  "T if FBOUND has TEST.

Tests are equal if they test the same input"
  (member test (get-fbound-plist-tests fbound)
          :test #'io-set-equalp))

;;;; Query stuff -------------------------------------------------------
;;; Our primary purpose here is to locate and find different
;;; tests. Without this things are pretty difficult. Because of the test
;;; framework model we are mostly based on a per package basis.
(declaim (ftype (function (possible-package)
                          (values &rest io-set))
                find-tested-symbols))

(defun find-tested-symbols (package-spec)
  "List symbols that have at least one test to run"
  (loop for x being the present-symbols in package-spec
       when (and (fboundp x) (fbound-plist-tests-p x)) collect x))


;;;; Running the tests
;;; Run iosets, log the results of these runs.

(defgeneric run-test-set (input io-set)
  (:documentation "Run a test set and get results")
  ;; Need to capture output, trace, handle errors
  (:method ((input io-set) (test function))
    "The easiest case, we get as input the objects we want to run a single test set"
    (let ((result-set (make-io-result)))
      (setf (result-value result-set)
            (apply test (io-set-input input)))
      ;; Set the end of the test, needs to be its own function
      (setf (result-end-time result-set)
            (get-universal-time))
      (log-test-result input result-set)
      result-set)))

(defgeneric log-test-result (object result)
  (:documentation "Log results to object's test log")
  (:method ((object io-log) (result io-result))
    "Log RESULT to object's result log"
    (push result (io-log-results object)))
  (:method ((object io-set) (result io-result))
    "Log RESULT to io-set's log."
    (log-test-result (io-set-log object) result)))

(defgeneric compare-last-test-result (fbound)
  ;; If no expected result, we should assume regression testing, which
  ;; means verify the result against test history
  (:documentation "Compare the last test result with the expected result.")
  (:method ((set io-set))
    (equal (result-value set)
           (result-value (first (io-log-results (io-set-log set))))))
  (:method ((fbound symbol))
    (declare (type fbound fbound))
    (mapcar #'compare-last-test-result
            (get-fbound-plist-tests fbound))))

;;; Making a pretty big assumption that the fbound object will have all
;;; the information we need to actually run a test
(defgeneric run-fbound-set (fbound)
  (:documentation "Run all tests on the set.")
  (:method ((fbound symbol))
    "Run all tests listed in FBOUND's plist."
    (if (fboundp fbound)
        (mapcar (lambda (test)
                  (declare (type io-set test))
                  (run-test-set test (symbol-function fbound)))
                (get-fbound-plist-tests fbound))
        (run-fbound-set (find-package fbound))))
  (:method ((fbound package))
    (mapcar #'run-time (find-tested-symbols fbound))))

(defgeneric run-time (object)
  ;; Keep in mind it only makes sense to ask about the runtime of the
  ;; last test run. Though a query about the whole test log could be
  ;; made using this by passing a list of io-sets in.
  (:documentation "Compute the runtime of the test.")
  (:method ((result io-result))
    "Return an integer with result run-time."
    (- (result-end-time result)
       (result-creation-time result)))
  (:method ((log io-log))
    "Return an integer of the last run io-result."
    (run-time (last-test-result log)))
  (:method ((set io-set))
    "Get last run-time for IO-SET."
    (run-time (io-set-log set)))
  (:method ((fbound-symbol symbol))
    "Get list of last test runtimes for FBOUND-SYMBOL."
    (if (fboundp fbound-symbol)
        (mapcar #'run-time (get-fbound-plist-tests fbound-symbol))
        (run-time (find-package fbound-symbol))))
  (:method ((package package))
    "Get runtimes for all tests in package"
    (mapcar #'run-time (find-tested-symbols package)))
  (:method ((lst list))
    "Get runtimes for every element in the list.

This returns a list with runtimes, note that this may mean sublists if
the list has an fbound symbol in it or a package."
    (mapcar #'run-time lst)))

(defgeneric last-test-result (object)
  (:documentation "Get the last io-result in OBJECT")
  (:method ((log io-log))
    "Last result in the log."
    (let ((log-entries (io-log-results log)))
      (when (single log-entries)
        (error "Run some tests! The log has no entries."))
      (first (io-log-results log))))
  (:method ((set io-set))
    (last-test-result (io-set-log set)))
  (:method ((result io-result))
    "Degenerate case, if we get an io-result, spit the same one back."
    result)
    (:method ((fbound-symbol symbol))
    "Get list of last test runtimes for FBOUND-SYMBOL."
    (if (fboundp fbound-symbol)
        (mapcar #'last-test-result (get-fbound-plist-tests fbound-symbol))
        (last-test-result (find-package fbound-symbol))))
  (:method ((package package))
    "Get runtimes for all tests in package"
    (mapcar #'last-test-result (find-tested-symbols package)))
  (:method ((lst list))
    "Get runtimes for every element in the list.

This returns a list with runtimes, note that this may mean sublists if
the list has an fbound symbol in it or a package."
    (mapcar #'last-test-result lst)))
