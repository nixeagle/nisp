(in-package :cl-user)

(defpackage #:nix-bot-system
  (:use :cl :asdf))

(in-package #:nix-bot-system)

(defsystem :nix-bot-tests
  :components ((:file "nix-bot-tests"))
  :depends-on (:FiveAM
               :nix-bot))