(defpackage #:nisp.i
  (:use :cl :iterate :split-sequence :nisp.util.packages :alexandria :eos
        :with-fbound)
  (:import-from :cl-irc :user #:nickname #:username #:realname))

(defpackage #:nisp.i-zeta
  (:use :cl :eos :with-fbound :iterate :alexandria))
(in-package :nisp.i)
