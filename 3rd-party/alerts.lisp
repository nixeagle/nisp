;;; Wed Mar 20 17:24:00 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; alerts.lisp

;;; ****************************************************************
;;; Alerts: A Convenient Debugging Status Indicator ****************
;;; ****************************************************************
;;;
;;; Written by Kenneth J Meltsner <meltsner@chipman.crd.ge.com>
;;;
;;; From http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/ext/alerts/alerts.cl
;;;
;;; Copying/distribution/modification are all allowed by original author.
;;; See: http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/ext/alerts/0.html
;;;
;;; Modifications made to make this compile on sbcl by James S.
;;; Copyright (C) 2010, released under the same terms as original author.

(in-package :cl-user)
(defpackage #:alerts
  (:use :common-lisp)
  (:export :set-alert-level :set-alert-stream :alert :query-user))
(in-package :alerts)

;;; ********************************
;;; Alert Functions ****************
;;; ********************************
;;; Alert functions

(defvar *alert-level* 0
  "If alert-level is nil, then expressions within an alert are not defined or
   invoked.  Use (set-alert-level nil) before functions are loaded or defined
   to do this.  If it is done afterwards, they will not be invoked, but
   their code will still exist.")

(defun set-alert-level (&optional (x 0))
  (when (numberp x) (setq *alert-level* x)))

(defvar *alert-stream* *standard-output*
  "*standard-output* is redirected to *alert-stream* for all alert output.
   If nil, output is sent to *standard-output*.")

(defun set-alert-stream (&optional x)
  (cond ((output-stream-p x) (setq *alert-stream* x))
	((null x) (setq *alert-stream* *error-output*))))

(defmacro alert (level &rest ops)
  "Alert user macro -- (alert level (operation1) (operation2)...)
   When *alert-level* is less than or equal to level, perform the specified 
   operations.  If LEVEL is t, always perform them (equivalent to progn) and
   ignore the alert-level setting.

   Typically:
	     level = 0     interesting comments of no real importance
             level = 1     minor status notes
             level = 2     major status notes
             level = 3     warnings
             level = 4     real problems
	     level = t     fatal errors

   If the first argument after the level is a string, there is an implicit
   format command.  (alert 4 \"~%Alert! ~A is bad\" arg) = 
                    (alert 4 (format t \"~%Alert! ~A is bad\" arg))"
  (when (stringp (car ops))
    (setq ops `((format t ,@ops))))
  (when (or (eq level t) (numberp *alert-level*))
    `(when ,(if (numberp level)
		`(and (numberp *alert-level*)
		      (>= ,level *alert-level*))
	      level)
       (let ((*standard-output* (or *alert-stream* *standard-output*)))
	 (fresh-line *standard-output*)
	 ,@ops
	 (force-output *standard-output*)))))


(defmacro query-user (&rest ops)
  "Query user macro -- (query (operation1) (operation2)...)

   If the first argument after the level is a string, there is an implicit
   format command.  (alert 4 \"~%Alert! ~A is bad\" arg) = 
                    (alert 4 (format t \"~%Alert! ~A is bad\" arg))"
  (when (stringp (car ops))
    (setq ops `((format t ,@ops))))
  `(progn ,@ops 
	  (force-output)
	  (read)))

;;; *EOF*
