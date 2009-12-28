;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10;  indent-tabs-mode: nil -*-
(in-package :functional-tests)

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
