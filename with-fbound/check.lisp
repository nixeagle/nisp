;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :Eos)

(defvar *test-dribble* t)

(defmacro with-*test-dribble* (stream &body body)
  `(let ((*test-dribble* ,stream))
     (declare (special *test-dribble*))
     ,@body))

(defmacro bind-run-state (requested-vars &body body)
  `(let ,requested-vars
     (declare (special ,@(mapcar 'car requested-vars)))
     ,@body))
(defmacro with-run-state (requested-vars &body body)
  `(locally (declare (special ,@requested-vars)) ,@body))

(defmethod (setf status) :after ((status (eql :unknown)) (test test-case))
  (setf (slot-value test 'sofar) ()))

(defclass test-case (testable-object)
  ((test-lambda :initarg :test-lambda :accessor test-lambda
		:documentation "The function to run.")
   (sofar :initform () :reader test-case-sofar
          :documentation "Tests run so far.")
   (runtime-package :initarg :runtime-package :accessor runtime-package
                    :documentation "By default it stores *package* from the time this test was defined (macroexpanded)."))
  (:documentation "A test case is a single, named, collection of
checks.

A test case is the smallest organizational element which can be
run individually. Every test case has a name, which is a symbol,
a description and a test lambda. The test lambda is a regular
funcall'able function which should use the various checking
macros to collect results.

Every test case is part of a suite, when a suite is not
explicitly specified (either via the :SUITE parameter to the TEST
macro or the global variable *SUITE*) the test is inserted into
the global suite named NIL.

Sometimes we want to run a certain test only if another test has
passed. FiveAM allows us to specify the ways in which one test is
dependent on another.

- AND Run this test only if all the named tests passed.

- OR Run this test if at least one of the named tests passed.

- NOT Run this test only if another test has failed.

FiveAM considers a test to have passed if all the checks executed
were successful, otherwise we consider the test a failure.

When a test is not run due to it's dependencies having failed a
test-skipped result is added to the results."))

(defmethod (setf test-case-sofar) (test-form (test test-case))
  "Push a new TEST-FORM onto TEST."
  (unless (member test-form (slot-value test 'sofar) :test #'equal)
    (setf (slot-value test 'sofar)
          (append (slot-value test 'sofar) (list test-form)))))
(defmethod (setf test-case-sofar) (test-form (test test-result))
  (setf (test-case-sofar test) test-form))
(defmethod test-case-sofar ((test test-result))
  (test-case-sofar (test-case test)))

(defclass test-result ()
  ((reason :accessor reason :initarg :reason :initform "reason")
   (docstring :accessor docstring :initarg :docstring :initform "docstring")
   (test-case :accessor test-case :initarg :test-case)
   (actual-value :accessor test-actual-value :initarg :actual-value
                 :initform 'no-result)
   (expected-value :accessor test-expected-value :initarg :expected-value
                   :initform 'no-result)
   (test-expr :accessor test-expr :initarg :test-expr))
  (:documentation "All checking macros will generate an object of type TEST-RESULT."))

(defclass test-passed (test-result) ()
  (:documentation "Class for successful checks."))

(defgeneric test-passed-p (object)
  (:method ((o t)) nil)
  (:method ((o test-passed)) t))

(define-condition check-failure (error)
  ((reason :accessor reason :initarg :reason :initform "reason")
   (test-case :accessor test-case :initarg :test-case)
   (test-expr :accessor test-expr :initarg :test-expr))
  (:documentation "Signaled when a check fails.")
  (:report  (lambda (c stream)
              (format stream "The following check failed: ~S~%~A."
                      (test-expr c) (reason c)))))

(defmacro process-failure (&rest args)
  `(progn
     (with-simple-restart (ignore-failure "Continue the test run.")
       (error 'check-failure ,@args))
     (add-result 'test-failure ,@args)))

(defclass test-failure (test-result) ()
  (:documentation "Class for unsuccessful checks."))

(defgeneric test-failure-p (object)
  (:method ((o t)) nil)
  (:method ((o test-failure)) t))

(defclass unexpected-test-failure (test-failure)
  ((actual-condition :accessor actual-condition :initarg :condition))
  (:documentation "Represents the result of a test which neither
passed nor failed, but signaled an error we couldn't deal
with.

Note: This is very different than a SIGNALS check which instead
creates a TEST-PASSED or TEST-FAILURE object."))

(defclass test-skipped (test-result) ()
  (:documentation "A test which was not run. Usually this is due
to unsatisfied dependencies, but users can decide to skip test
when appropiate."))

(defgeneric test-skipped-p (object)
  (:method ((o t)) nil)
  (:method ((o test-skipped)) t))

(defun add-result (result-type &rest make-instance-args)
  "Create a TEST-RESULT object of type RESULT-TYPE passing it the
  initialize args MAKE-INSTANCE-ARGS and adds the resulting
  object to the list of test results."
  (with-run-state (result-list current-test)
    (let ((result (apply #'make-instance result-type
                         (append make-instance-args (list :test-case current-test)))))
      (setf (test-case-sofar current-test)
            (getf make-instance-args :test-expr "no test-expression."))

      (format *test-dribble* (etypecase result
                               (test-passed             ".") (test-skipped "s")
                               (unexpected-test-failure "X") (test-failure "f")))
      (push result result-list))))

(defgeneric find-predicate (arg1 arg2))
(macrolet ((define-samep (type1 predicate type2)
             `(defmethod find-predicate ((arg1 ,type1) (arg2 ,type2))
                ,(format nil "Returns predicate: ~A" (symbol-name predicate))
                ',predicate)))
  (define-samep string string= string)
  (define-samep character char= character)
  (define-samep number = number)
  (define-samep t equal t))

(defmacro is (test &rest reason-args)
  "The DWIM checking operator.

If TEST returns a true value a test-passed result is generated,
otherwise a test-failure result is generated. The reason, unless
REASON-ARGS is provided, is generated based on the form of TEST:

 (predicate expected actual) - Means that we want to check
 whether, according to PREDICATE, the ACTUAL value is
 in fact what we EXPECTED.

 (predicate value) - Means that we want to ensure that VALUE
 satisfies PREDICATE.

 Wrapping the TEST form in a NOT simply preducse a negated reason
 string."
  (assert (listp test) (test) "Argument to IS must be a list, not ~S" test)
  (let (bindings effective-test default-reason-args)
    (with-gensyms (e a v p modified-test result)
      (flet ((process-entry (predicate expected actual &optional negatedp)
               ;; make sure EXPECTED is holding the entry that starts with 'values
               (when (and (consp actual) (eq (car actual) 'values))
                 (assert (not (and (consp expected) (eq (car expected) 'values))) ()
                         "Both the expected and actual part is a values expression.")
                 (rotatef expected actual))
               (let (setf-forms)
                 (if (and (consp expected) (eq (car expected) 'values))
                     (progn
                       (setf expected (copy-list expected))
                       (setf setf-forms (loop :for cell := (rest expected) :then (cdr cell)
                                              :for i :from 0
                                              :while cell
                                              :when (eq (car cell) '*)
                                              :collect `(setf (elt ,a ,i) nil)
                                              :and :do (setf (car cell) nil)))
                       (setf bindings (list (list e `(list ,@(rest expected)))
                                            (list a `(multiple-value-list ,actual)))))
                     (setf bindings (list (list e expected) (list a actual)
                                          (list p `',predicate))))
                 (setf effective-test `(progn
                                         ,@setf-forms
                                         (setf ,p (if (eq 'find-predicate ,p)
                                                    (funcall #'find-predicate
                                                             ,e ,a)
                                                    ,p))
                                         ,(if negatedp
                                             `(not ,(list 'funcall p e a))
                                             (list 'funcall p e a))))))
             (reverse-process-entry (predicate expected actual &optional negatedp)
               ;; make sure EXPECTED is holding the entry that starts with 'values
               (when (and (consp actual) (eq (car actual) 'values))
                 (assert (not (and (consp expected) (eq (car expected) 'values))) ()
                         "Both the expected and actual part is a values expression.")
                 (rotatef expected actual))
               (let (setf-forms)
                 (if (and (consp expected) (eq (car expected) 'values))
                     (progn
                       (setf expected (copy-list expected))
                       (setf setf-forms (loop :for cell := (rest expected) :then (cdr cell)
                                              :for i :from 0
                                              :while cell
                                              :when (eq (car cell) '*)
                                              :collect `(setf (elt ,a ,i) nil)
                                              :and :do (setf (car cell) nil)))
                       (setf bindings (list (list a `(multiple-value-list ,actual))
                                            (list e `(list ,@(rest expected))))))
                     (setf bindings (list (list a actual) (list e expected)
                                          (list p `',predicate))))
                 (setf effective-test `(progn
                                         ,@setf-forms
                                         (setf ,p (if (eq 'find-predicate ,p)
                                                    (funcall #'find-predicate
                                                              ,a ,e)
                                                    ,p))
                                         (multiple-value-bind (,result)
                                             (funcall ,p ,a ,e)
                                           ,(if negatedp
                                               `(not ,result)
                                               result)))))))
        (list-match-case test
          ((not (typep ?value ?type))
           (setf bindings (list (list v ?value) (list a ?value)
                                (list e ?type))
                 effective-test `(not (typep ,v ,e))
                 modified-test nil
                 default-reason-args
                 (list "~S evaluated to ~S, which is a ~S (it should not be)"
                       `',?value v `,?type)))
          ((not (comparable (?predicate ?expected ?actual)))
           (reverse-process-entry ?predicate ?expected ?actual t)
           (setf modified-test `(list ,p ',?actual ',?expected)
                 default-reason-args
                 (list "~S evaluated to ~S, which is not ~S to ~S. (it should not be)"
                       `',?actual e p a)))
          ((not (?predicate ?expected ?actual))
           (process-entry ?predicate ?expected ?actual t)
           (setf modified-test `(list ,p ',?expected ',?actual)
                 default-reason-args
                 (list "~S evaluated to ~S, which is ~S to ~S (it should not be)"
                       `',?actual a p e)))
          ((not (?satisfies ?value))
           (setf bindings (list (list v ?value) (list e nil)
                                (list a ?value))
                 effective-test `(not (,?satisfies ,v))
                 modified-test nil
                 default-reason-args
                 (list "~S evaluated to ~S, which satisfies ~S (it should not)"
                       `',?value v `',?satisfies)))
          ((typep ?value ?type)
           (setf bindings (list (list v ?value)
                                (list e ?type) (list a ?value))
                 effective-test `(typep ,v ,e)
                 modified-test nil
                 default-reason-args
                 (list "~S evaluated to ~S, which is not a ~S"
                       `',?value v `,?type)))
          ((comparable (?predicate ?expected ?actual))
           (reverse-process-entry ?predicate ?expected ?actual)
           (setf default-reason-args
                 (list "~S evaluated to ~S, which is not ~S to ~S."
                       `',?actual e p a)
                 modified-test `(list ,p ',?actual ',?expected)))
          ((?predicate ?expected ?actual)
           (process-entry ?predicate ?expected ?actual)
           (setf modified-test `(list ,p ',?expected ',?actual)
                 default-reason-args
                 (list "~S evaluated to ~S, which is not ~S to ~S."
                       `',?actual a p e)))
          ((?satisfies ?value)
           (setf bindings (list (list v ?value) (list e nil)
                                (list a ?value))
                 effective-test `(,?satisfies ,v)
                 modified-test nil
                 default-reason-args
                 (list "~S evaluated to ~S, which does not satisfy ~S"
                       `',?value v `',?satisfies)))
          (?_
           (setf bindings '()
                 modified-test nil
                 effective-test test
                 default-reason-args (list "~S was NIL." `',test)))))
      `(let ,bindings
         (if ,effective-test
             (add-result 'test-passed :test-expr (or ,modified-test ',test)
                           :actual-value ,a
                           :expected-value ,e
                           :docstring ,(format nil (or (car reason-args) "")))
             (process-failure :reason (format nil ,@default-reason-args)
                              :docstring (format nil "~A" ,@(or reason-args
                                                          (list "")))
                                :actual-value ,a
                                :expected-value ,e
                                :test-expr (or ,modified-test ',test)))))))

(defmacro skip (&rest reason)
  "Generates a TEST-SKIPPED result."
  `(add-result 'test-skipped :reason (format nil ,@reason)))

(defmacro is-true (condition)
  "Like IS this check generates a pass if CONDITION returns true
  and a failure if CONDITION returns false. Unlike IS this check
  does not inspect CONDITION to determine how to report the
  failure."
  `(if ,condition
       (add-result 'test-passed :test-expr ',condition)
       (process-failure
        :reason (format nil "~S did not return a true value" ',condition)
        :test-expr ',condition)))

(defmacro is-false (condition &rest reason-args)
  "Generates a pass if CONDITION returns false, generates a
  failure otherwise. Like IS-TRUE, and unlike IS, IS-FALSE does
  not inspect CONDITION to determine what reason to give it case
  of test failure"
  (with-gensyms (value)
    `(let ((,value ,condition))
       (if ,value
           (process-failure
            :reason ,(if reason-args
                         `(format nil ,@reason-args)
                         `(format nil "~S returned the value ~S, which is true" ',condition ,value ))
            :test-expr ',condition)
           (add-result 'test-passed :test-expr ',condition)))))

(defmacro signals (condition-spec &body body)
  "Generates a pass if BODY signals a condition of type
CONDITION. BODY is evaluated in a block named NIL, CONDITION is
not evaluated."
  (let ((block-name (gensym)))
    (destructuring-bind (condition &optional reason-args)
        (ensure-list condition-spec)
      `(block ,block-name
         (handler-bind ((,condition (fun (add-result 'test-passed :test-expr ',condition)
                                         (return-from ,block-name t))))
           (block nil ,@body))
         (process-failure
          :reason (format nil "Failed to signal a ~S" ',condition)
          :docstring `(format nil "~A" ,@(or reason-args "docstring"))
          :expected-value ',condition
          :test-expr ',condition)
         (return-from ,block-name nil)))))

(defmacro finishes (&body body)
  "Generates a pass if BODY executes to normal completion. In
other words if body does signal, return-from or throw this test
fails."
  `(let (ok)
     (unwind-protect (progn ,@body (setf ok t))
       (if ok
           (add-result 'test-passed :test-expr ',body)
           (process-failure
            :reason (format nil "Test didn't finish")
            :test-expr ',body)))))


(defmacro pass (&rest message-args)
  "Simply generate a PASS."
  `(add-result 'test-passed :test-expr ',message-args
               ,@(when message-args `(:reason (format nil ,@message-args)))))

(defmacro fail (&rest message-args)
  "Simply generate a FAIL."
  `(process-failure :test-expr ',message-args
                    ,@(when message-args `(:reason (format nil ,@message-args)))))

(defparameter *verbose-failures* t
  "T if we should print the expression failing, NIL otherwise.")

(defgeneric explain (explainer results &optional stream recursive-depth)
  (:method ((exp detailed-text-explainer) results
            &optional (stream *test-dribble*) (recursive-depth 0))
    (multiple-value-bind (num-checks passed num-passed passed%
                                     skipped num-skipped skipped%
                                     failed num-failed failed%
                                     unknown num-unknown unknown%)
        (partition-results results)
      (declare (ignore passed))
      (flet ((output (&rest format-args)
               (format stream "~&~vT" recursive-depth)
               (apply #'format stream format-args)))
        (when (zerop num-checks)
          (output "Didn't run anything...huh?")
          (return-from explain nil))
        (output "Did ~D check~P.~%" num-checks num-checks)
        (output "   Pass: ~D (~2D%)~%" num-passed passed%)
        (output "   Skip: ~D (~2D%)~%" num-skipped skipped%)
        (output "   Fail: ~D (~2D%)~%" num-failed failed%)
        (when unknown (output "   UNKNOWN RESULTS: ~D (~2D)~%" num-unknown unknown%))
        (terpri stream)
        (when failed
          (output "Failure Details:~%")
          (dolist (f (reverse failed))
            (output "--------------------------------~%")
            (output "~A ~@{[~A]~}: ~%"
                    (name (test-case f))
                    (description (test-case f)))
            (output "  ~@{[~A]~}~%~%" (docstring f))
            (output "     ~A~%~%" (reason f))
            (output "-- -- Expected -- --~%")
            (output "~A" (with-output-to-string (stream)
                           (describe (test-expected-value f) stream)))
            (output "~%~%-- --  Actual  -- --~%")
            (output "~A" (with-output-to-string (stream)
                           (describe (test-actual-value f) stream)))
            (output "~%~%")
            (when (and *verbose-failures* (test-expr f))
              (output "EXPR:  ~S~%" (test-expr f)))
            (output "--------------------------------~%"))
          (terpri stream))
        (when skipped
          (output "Skip Details:~%")
          (dolist (f skipped)
            (output "~A ~@{[~A]~}: ~%"
                    (name (test-case f))
                    (description (test-case f)))
            (output "    ~A.~%" (reason f)))
          (terpri stream)))))
  (:method ((exp simple-text-explainer) results
            &optional (stream *test-dribble*) (recursive-depth 0))
    (multiple-value-bind (num-checks passed num-passed passed%
                                     skipped num-skipped skipped%
                                     failed num-failed failed%
                                     unknown num-unknown unknown%)
        (partition-results results)
      (declare (ignore passed passed% skipped skipped% failed failed% unknown unknown%))
      (format stream "~&~vTRan ~D checks, ~D passed" recursive-depth num-checks num-passed)
      (when (plusp num-skipped)
        (format stream ", ~D skipped " num-skipped))
      (format stream " and ~D failed.~%" num-failed)
      (when (plusp num-unknown)
        (format stream "~vT~D UNKNOWN RESULTS.~%" recursive-depth num-unknown)))))

(defun partition-results (results-list)
  (let ((num-checks (length results-list)))
    (collect (passed skipped failed unknown)
      (dolist (result results-list)
        (typecase result
          (test-passed  (passed result))
          (test-skipped (skipped result))
          (test-failure (failed result))
          (otherwise    (unknown result))))
      (if (zerop num-checks)
          (values 0 nil 0 0 nil 0 0 nil 0 0 nil 0 0)
          (values
           num-checks
           (passed)  (length (passed))  (floor (* 100 (/ (length (passed))  num-checks)))
           (skipped) (length (skipped)) (floor (* 100 (/ (length (skipped)) num-checks)))
           (failed)  (length (failed))  (floor (* 100 (/ (length (failed))  num-checks)))
           (unknown) (length (unknown)) (floor (* 100 (/ (length (failed))  num-checks))))))))


(defgeneric run-test-lambda (test)
  (:method ((test test-case))
    (with-run-state (result-list)
      (bind-run-state ((current-test test))
        (labels ((abort-test (e)
                   (add-result 'unexpected-test-failure
                               :test-expr nil
                               :test-case test
                               :reason (format nil "Unexpected Error: ~S~%~A." e e)
                               :condition e))
                 (run-it ()
                   (let ((result-list '()))
                     (declare (special result-list))
                     (handler-bind ((check-failure (lambda (e)
                                                     (declare (ignore e))
                                                     (unless *debug-on-failure*
                                                       (invoke-restart
                                                        (find-restart 'ignore-failure)))))
                                    (error (lambda (e)
                                             (unless (or *debug-on-error*
                                                         (typep e 'check-failure))
                                               (abort-test e)
                                               (return-from run-it result-list)))))
                       (restart-case
                           (let ((*readtable* (copy-readtable))
                                 (*package* (runtime-package test)))
                             (funcall (test-lambda test)))
                         (retest ()
                           :report (lambda (stream)
                                     (format stream "~@<Rerun the test ~S~@:>" test))
                           (return-from run-it (run-it)))
                         (ignore ()
                           :report (lambda (stream)
                                     (format stream "~@<Signal an exceptional test failure and abort the test ~S.~@:>" test))
                           (abort-test (make-instance 'test-failure :test-case test
                                                      :reason "Failure restart."))))
                       result-list))))
          (let ((results (run-it)))
            (setf (status test) (results-status results)
                  result-list (nconc result-list results))))))))