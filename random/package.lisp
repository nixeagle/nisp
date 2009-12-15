(defpackage #:nisp-util
  (:use :cl :lift :metatilities)
  (:export #:define-constant
           #:strip-newlines
           #:ascii-character-range))

(defpackage #:nisp-random
  (:use :cl :lift :nisp-util
        :nisp-util))