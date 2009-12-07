;;; Redefine 5am's def-fixture so its not broken.
#+5am
(progn
  (in-package :FiveAM)

  (setq *debug-on-error* nil)
  (defparameter *debug-on-failure* nil)

  (defmacro def-fixture (name args &body body)
    "Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
&BODY to specify where the body should go.

See Also: WITH-FIXTURE

*note* that this overrides the default 5am DEF-FIXTURE. 5am's
DEF-FIXTURE evals itself too much causing a SBCL compiler warning
about redefining a symbol."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (rem-fixture ',name)
       (setf (get-fixture ',name) (cons ',args ',body))
       ',name)))