(defpackage #:nisp.i 
  (:use :cl :iterate :split-sequence :nisp.util.packages :alexandria :eos
        :with-fbound)
  (:import-from :cl-irc :user #:nickname #:username #:realname))
(in-package :nisp.i)
