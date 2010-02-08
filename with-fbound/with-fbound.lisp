(defpackage #:with-fbound
  (:use :cl :eos :iterate)
  (:nicknames :with-fbound)
  (:export :with-fbound))

(in-package :with-fbound)

(defmacro with-fbound ((fbound-call &key predicate) &body args)
  "Compare FBOUND-CALL using PREDICATE using series of ARGS.

This macro has some funny destructuring.
  - If the first of ARGS is a string, its taken as the reason/docstring. 
    This defaults to \"No reason given.\". However when a docstring is given
    this docstring will apply for all argument/return value pairs until another
    docstring is given.
  - The next of ARGS _must_ be a list. These are supplied to FBOUND as arguments.
  - Finally the next member of ARGS is used as the expected return result.
    The correct predicate to use is looked up from `with-fbound-samep' unless
    the keyword PREDICATE is non-nil.
    _however_ if the last member is one of the following, magic happens:
    - :signals or :should-signal followed by condition-spec:
      Expand into (signals (condition-spec <generated formatstring> docstring))
    - :finishes or :should-finish
      Expand into (finishes ...)
    - :expects or :should-be followed by type-spec
      Expand into (is (typep <function/arguments> type-spec) docstring)
    - :satisfies or :should-satisfy followed by predicate
      Expand into (is (predicate <function/arguments>) docstring)"
  `(progn
     ,@(iter (with docs = nil)
             (with is-test = nil)
             (with is-tests = nil)
             (for pretty-error-info = nil)
             (for arg-lists = nil)
             (for expected-result = nil)
             (for moved-past-docstring-p = nil) ;For pretty error messages.
             (for expect-signals-argument-p = nil)
             (generating arg :in args)
             (next arg)
             (for count :upfrom 1)
             (when (stringp arg)
               (setq docs arg)
               (next arg))
             (setq moved-past-docstring-p t)
             (when (listp arg)
               (setq arg-lists arg)
               (setq expected-result (next arg))
               #+ () (print `',expected-result)
               (setq is-test
                     (cond
                       ((or (eq :should-signal expected-result)
                            (eq :signals expected-result))
                        ;; Want to check for a signal/condition.
                        (let ((signals-arg (next arg)))
                          (assert (symbolp signals-arg)
                                  nil
                                  "Expect a symbol for <condition>~%got: ~:S" signals-arg)
                          `(signals (,signals-arg
                                     ,docs)
                             (,fbound-call ,@arg-lists))))
                       ((or (eq :should-finish expected-result)
                            (eq :finishes expected-result))
                        `(finishes (,fbound-call ,@arg-lists)))
                       ((or (eq :satisfies expected-result)
                            (eq :should-satisfy expected-result))
                        (let ((satisfies-arg (next arg)))
                          (let ((notp
                                 (and (consp satisfies-arg)
                                      (eq 'not (first satisfies-arg))
                                      (setq satisfies-arg
                                            (second satisfies-arg)))))
                                        (assert (symbolp satisfies-arg)
                                                      nil
                                                      "Expect a symbol that stands for a predicate of one argument for <predicate>~%got: ~:S" satisfies-arg)
                            (if notp
                                `(is (not (,satisfies-arg
                                           (,fbound-call ,@arg-lists)))
                                     ,docs)
                                `(is (,satisfies-arg
                                      (,fbound-call ,@arg-lists))
                                     ,docs)))))
                       ((or (eq :expects expected-result)
                            (eq :should-be expected-result))
                        (let ((type-arg (next arg)))
                          (let ((notp (and (consp type-arg)
                                           (eq 'not (first type-arg))
                                           (setq type-arg
                                                 (second type-arg)))))
                            (assert (symbolp type-arg)
                                    nil
                                    "Expect a symbol that stands for an expected result type.~%got: ~:S" type-arg)
                            (if notp
                                `(is (not (typep (,fbound-call ,@arg-lists) ',type-arg))
                                     ,docs)
                                `(is (typep (,fbound-call ,@arg-lists) ',type-arg)
                                     ,docs)))))
                       ((and (consp expected-result)
                             (eq 'not (first expected-result))
                             (not (eq 'quote (first expected-result)))
                             (consp (second expected-result))
                             (fboundp (first (second expected-result)))
                             (setq expected-result 
                                   (second expected-result)))
                        `(is (not (eos::comparable
                                   (eos::find-predicate ,expected-result
                                          (,fbound-call ,@arg-lists))))
                             ,docs))
                       ((and (consp expected-result)
                             (eq 'not (first expected-result))
                             (setq expected-result
                                   (car (rest expected-result))))
                        `(is (not (eos::find-predicate ,expected-result
                                          (,fbound-call
                                            ,@arg-lists)))
                             ,docs))
                       ((and (listp expected-result)
                             (not (eq 'quote (first expected-result)))
                             (fboundp (first expected-result)))
                        `(is (eos::comparable
                              (eos::find-predicate ,expected-result
                                     (,fbound-call ,@arg-lists)))
                             ,docs))
                       ((not predicate)
                        `(is (eos::find-predicate ,expected-result
                                     (,fbound-call
                                       ,@arg-lists))
                             ,docs))
                       (t `(is (,predicate ,expected-result
                                           (,fbound-call ,@arg-lists))
                               ,docs))))
               (push is-test is-tests)
               (setq pretty-error-info (format nil "Docstring: ~S~%Arg List: ~:S~%Expected result: ~S~%" docs arg-lists expected-result)))
             (finally (if moved-past-docstring-p
                          (assert (= count (length is-tests))
                                  nil "Passed in arguments do not properly destructure.~%Make sure that each set of arguments has a corresponding expected result.~%~%Hanging argument was: ~A~%~%Make sure there is a return value for the hanging argument!~%~%Correctly-parsed arguments were:~%~%~{~A~%~}~%Raw Arguments were: ~%~A"  (format nil "~:S" arg-lists) pretty-error-info (format nil "~:S" args))
                          (assert (= count (length is-tests))
                                  nil "Trailing reason/docstring ~S with no associated arglist or expected return value." docs))
                      (return (nreverse (eos::ensure-list is-tests)))))))