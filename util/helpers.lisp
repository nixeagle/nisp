(in-package :nisp.util-system)
(defpackage #:nisp.util-helpers
  (:use :common-lisp :iterate :nisp.util-types)
  (:nicknames :h :helpers)
  (:shadow :time))
(in-package :nisp.util-helpers)

(defun export-some-internals (package-name)
  (iter (for (symbol state) :in-packages package-name :having-access (:internal))
        (when (or (type-specifier-p symbol)
                  (fboundp symbol))
          (export symbol package-name)
          (collect symbol))))

(defun unintern-externals-from (package &optional (in-package *package*))
  "Remove all exports from PACKAGE in FROM-PACKAGE."
  (iter (for (symbol)
             :in-packages package
             :having-access (:external))
        (collect symbol)
        (unintern symbol in-package)))

(defmacro compare-clock (iteration-count &rest forms)
  (alexandria:with-gensyms (whole-result)
    (format t "Doing ~A iterations.~%" (expt 2 iteration-count))
    `(iter (for ,whole-result
                in (list ,@(iter
                            (for form in forms)
                            (collect
                                `(clock ,form ,(expt 2 iteration-count))))))
           (collect ,whole-result))))

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