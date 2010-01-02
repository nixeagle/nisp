;; system definition file
(in-package :cl-user)
(defpackage #:nisp-safe-system
  (:use :cl :asdf))
(in-package :nisp-safe-system)

(defsystem :nisp-safe
  :version "0.0.10"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :depends-on (:lift
               :cl-walker
               :nistilities)
  :components
  ((:module "safe"
            :serial t
            :components
            ((:file "package")
             (:file "nisp-introspect")
             (:file "empty-package")
             (:file "arithmetic")
             (:file "readtable")
             (:file "safe-package")
             (:file "safe")
             (:file "safe-tests")))))

(defpackage #:nispbot-system
  (:use :cl :asdf))
(in-package #:nispbot-system)

(defsystem :nisp
  :version "0.0.10"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Nixeagle's random lisp experiments"
  :depends-on (:functional-tests
               :cl-ppcre
               :metatilities
               :nistilities
               :nisp.ldap
               :nisp.8b-ldap
               :cl-irc
               :nisp-safe
               :trivial-timeout
               :trivial-shell)
  :serial t
  :components
  ((:file "main-test-suite")
   (:file "package")
   (:file "nisp-asdf")
   (:file "config-dist")
   (:file "config")
   (:file "nispbot")
   (:file "nispbot-tests")
   (:file "nisp-dev-helper")))

;; (defsystem :nispbot
;;   :author "James S <dev@nixeagle.org>"
;;   :maintainer "James S <dev@nixeagle.org>"
;;   :license "GPLv2 or later"
;;   :description "irc bot"
;;   :depends-on (:lift
;;                :cl-ppcre
               
;;                :trivial-timeout
;;                :nisp)


;;   :serial t
;;   :components
;;   )

;; (defsystem #:nisp-dev-helper
;;   :depends-on (:lift :nisp :nispbot :trivial-shell)
;;   :components ((:file "nisp-dev-helper")))


;;   :properties (((#:albert #:use-temporary-files) . nil)
;; 		 ((#:albert #:output-dir) . "Docs-Nisp/")
;; 		 ((#:albert #:formats) . ("html"))
;; 		 ((#:albert #:docbook #:template) . "book")
;; 		 ((#:albert #:html #:output-dir) . "HTMLDocs")
;; 		 ((#:albert #:presentation #:funcallable #:calledby) t)
;; ;		 ((#:albert #:docbook #:cvs-viewurl) . "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/langband/langband/")
;; ;		 ((#:albert #:docbook #:cvs-tag) . "HEAD")
;; ;		 ((#:albert #:html #:cvs-viewurl) . "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/langband/langband/")
;; ;		 ((#:albert #:html #:cvs-tag) . "HEAD")
;; 		 )

  ;; :properties ((#:author-email . "dev@nixeagle.org")
  ;;              (#:date . "Future")
  ;;              ((#:albert #:output-dir) . "albert-docs/")
  ;;              ((#:albert #:formats) . '("html"))
  ;;              (("albert" "presentation" "class" "related-methods") . t)
  ;;              ((#:albert #:docbook #:template) . "book")
  ;;              ((#:albert #:docbook #:bgcolor) . "white")
  ;;              ((#:albert #:docbook #:textcolor) . "black")
  ;;              )