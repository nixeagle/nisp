(require :asdf)
(pushnew "/usr/share/common-lisp/systems/" asdf:*central-registry*)

(pushnew "/home/james/lisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/asdf/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/emacs/slime/" asdf:*central-registry* :test #'equal)
(pushnew "/home/james/lisp/nisp/emacs/slime/contrib/" asdf:*central-registry* :test #'equal)
#+:ccl
(pushnew "/home/james/lisp/asdf-install/asdf-install/"
         asdf:*central-registry* :test #'equal)
(asdf:load-system :asdf-install)

(asdf:load-system :swank)
(asdf:load-system :eos)


(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)
                   (compilation-speed 0)))

(in-package :cl-user)

(asdf:operate 'asdf:load-op :nisp.user)

(in-package :nisp.user)