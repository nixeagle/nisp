;(require :asdf)
(pushnew "/usr/share/common-lisp/systems/" asdf:*central-registry*)

(pushnew "/home/james/lisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/asdf/" asdf:*central-registry* :test #'equal)

;;; debian ecl asdf install does not work right.
;;; it has a problem with trying to load sbcl specific things.
#+:ecl
(pushnew "/home/james/lisp/asdf-install/asdf-instal/" asdf:*central-registry*)
#+ecl
(load "/home/james/lisp/asdf-install/asdf-instal/load-asdf-install.lisp")

;;; gcl is cltl1, so asdf-install probably won't work
;;; ecl seems to not have asdf preloaded yet.
#+ (not (or gcl ecl))
(asdf:load-system :asdf-install)

;(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf asdf-install))

;(save-lisp-and-die "/home/james/sbcl.core-for-slime")
;(save-lisp-and-die "/home/nixeagle/lisp/sbcl.core-for-slime")

#-:mk-defsystem (load "/home/james/lisp/external-systems/defsystem.lisp")  
(mk:add-registry-location "/home/james/.asdf-install-dir/systems/")

;(declaim (optimize sb-cover:store-coverage-data))

;(load "/home/james/lisp/scheme/r6rs.asd")


#+sbcl
(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
#+ (or)
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

;;; If I have 5am and sbcl running I want to ignore redefinition warnings
;;; due to a bug with how it handles redefining tests.
#+ (and sbcl 5am)
  (setq sb-ext:*muffled-warnings* 'sb-kernel::redefinition-warning)

#+sbcl
(progn
  (setq sb-ext:*efficiency-note-cost-threshold* 2)
  (setq sb-ext:*derive-function-types* t))