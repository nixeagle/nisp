(defpackage #:nisp-dev-helper
  (:use :cl :nispbot)
  (:export #:start-nispbot-instance))

(in-package :nisp-dev-helper)

(defun start-nispbot-instance (&optional (nick nispbot-config::*nickname*))
  (setq nispbot::*nispbot* (nispbot::make-irc-bot nick "irc.eighthbit.net"))
  (irc:start-background-message-handler nispbot::*nispbot*)
  (sleep 3) 
  (nispbot::join-all-channels nispbot::*nispbot*)
  (nispbot::reset-command-hook nispbot::*nispbot*))

(defvar nispbot::*freenode*)
(defun start-freenode-instance ()
  "Quickie to get something up on freenode"
  (setq nispbot::*freenode*
        (irc:connect :connection-type 'nispbot::irc-bot
                     :nickname nispbot-config::*nickname*
                     :server "irc.freenode.net"
                     :password nispbot-config::*freenode-password*))
  (irc:start-background-message-handler nispbot::*freenode*)
  (sleep 3)
  (irc:join nispbot::*freenode* "#botters")
  (nispbot::reset-command-hook nispbot::*freenode*))

#+nil (defun generate-html-output ()
    (progn
      (write "")
      (setf (lift::test-result-property *test-result* :style-sheet) "test-style.css")
      (setf (lift::test-result-property *test-result* :title) "nisp")
      (setf (lift::test-result-property *test-result* :if-exists) :supersede)
      (lift::test-result-report *test-result* #p "/home/james/lisp/nisp/lift-tests/html/report" :html)
      (setf (lift::test-result-property *test-result* :style-sheet) "test-style.css")
      (setf (lift::test-result-property *test-result* :title) "nisp")
      (setf (lift::test-result-property *test-result* :if-exists) :supersede)
      
      (lift::test-result-report *test-result* (concatenate 'string "/home/james/lisp/nisp/lift-tests/save/report") :save)
      
      (update-lift-nisp)))

#+nil (defun update-lift-nisp ()
  "Currently a quick hack to upload test restuls and data using git to track history. This beats scp in several ways.

  1. we get history tracking

  2. we get compressed transfers as we only need send what changed"

  (trivial-shell::shell-command "cd /home/james/lisp/nisp/lift-tests/; git status; git add --all; git commit -m \"Automatic commit\"; git push vps:repos/nisp-tests.git master:master"))


;; (defun generate-tinaa-docs ()
;;   (use-package :tinaa)
;;   (tinaa:document-system
;;    'asdf-system :nisp "/home/james/lisp/nisp/lift-tests/tinaa/")
;#+ (or)   (trivial-shell::shell-command "scp -r /home/james/lisp/nisp/tinaa/* vps:paste/nisp/tinaa/"))

;;;;Old code
 ;; (trivial-shell::shell-command "scp /home/james/lisp/nisp/lift-tests/html/* vps:paste/nisp/")
 ;;      (trivial-shell::shell-command "scp /home/james/lisp/nisp/lift-tests/save/* vps:paste/nisp/save/")