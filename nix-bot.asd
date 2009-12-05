;; system definition file


(in-package :cl-user)

(defpackage #:nix-bot-system
  (:use :cl :asdf))

(in-package #:nix-bot-system)

(defsystem :nix-bot
  :version "0.0.0"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:cl-irc)
  :serial t
  :components
  ((:file "nix-bot")))