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
  :depends-on (:nisp
               :cl-irc
               :iterate
               :metabang-bind
               :functional-tests
               :cl-ppcre
               :nistilities
               :nisp.ldap
               :nisp.8b-ldap
               :closer-mop
               :nisp-safe
               :trivial-timeout
               :trivial-shell)
  :serial t
  :components
  ((:file "config-dist")
   (:file "config")
   (:file "types")
   (:file "nispbot")
   (:file "nispbot-tests")))