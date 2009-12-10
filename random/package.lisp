(defpackage #:nisp-util
  (:use :cl :lift))

(defpackage #:nisp-random
  (:use :cl #+5am :5am :lift
        :nisp-util))