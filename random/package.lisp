(defpackage #:nisp-util
  (:use :cl #+5am :5am))

(defpackage #:nisp-random
  (:use :cl #+5am :5am
        :nisp-util))