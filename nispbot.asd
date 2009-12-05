;; system definition file


(in-package :cl-user)

(defpackage #:nispbot-system
  (:use :cl :asdf))

(in-package #:nispbot-system)

(defsystem :nispbot
  :version "0.0.0"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:cl-ppcre
               :cl-irc)
  :serial t
  :components
  ((:file "nispbot")))