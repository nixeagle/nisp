(defpackage #:nisp.dwim.handle-otherwise
  (:use :cl)
  (:export #:handle-otherwise))

(in-package :nisp.dwim.handle-otherwise)


;;; Snagged from hu.dwim.util. Its in its own package so cl-walker can
;;; depend on that rather then on all of hu.dwim.util which is huge! Of
;;; course there is the small problem of "no documentation" but at least
;;; I don't need to pull in 100 dependencies.
;;;
;;; The only meaningful change I did was to prefix def with hu.dwim.def
;;; instead of using that package.
(hu.dwim.def:def (function ioe) handle-otherwise (otherwise)
  (cond
    ((eq otherwise :error)
     (error "Otherwise assertion failed"))
    ((and (consp otherwise)
          (member (first otherwise) '(:error :warn) :test #'eq))
     (assert (not (null (rest otherwise))))
     (apply (ecase (first otherwise)
              (:error #'error)
              (:warn  #'warn))
            (rest otherwise)))
    ((functionp otherwise)
     (funcall otherwise))
    (t
     otherwise)))