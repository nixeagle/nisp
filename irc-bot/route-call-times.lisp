;;; This code is strongly debug only
;;;
;;; Basically everything we ever route gets shoved into here with how long
;;; it took, what the result was, and what args we were given... so in
;;; theory that is enough to reproduce the input
(in-package :nisp.i)
(defvar *route-call-times* ()
  "List of execution times with routing arguments.")
(defgeneric route-call-times-info (what))
(defmethod route-call-times-info ((what (eql :times)))
  (mapcar (compose #'car #'last) *route-call-times*))
(defmethod route-call-times-info ((what (eql :results)))
  (mapcar (curry #'nth 5) *route-call-times*))
(defmethod route-call-times-info ((what (eql :args)))
  (mapcar (rcurry #'butlast 2) *route-call-times*))

;;; END
