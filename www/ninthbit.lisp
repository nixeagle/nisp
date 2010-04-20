;;; For ninthbit.net
(in-package :www)

(defun primary-acceptor ()
  #+nisp-devel
  www::*nisp-8080-acceptor*
  #+nisp-vps
  (assoc-value cl-user::*root-ports* 80))



(defparameter *9b*
  (hunchentoot-vhost:make-virtual-host "9b"
                                       '(#+nisp-vps "ninthbit.net"
                                         #+nisp-devel "9b-dev"
                                         #+nisp-devel "nix.ninthbit.net")
                                       :server (primary-acceptor)))

(hunchentoot-vhost:define-easy-virtual-handler *9b*
    (9b-main-page :uri "/") ()
  "Welcome to ninthbit.net. The irc network is at irc.ninthbit.net.")

#+nisp-devel
(hunchentoot-vhost:define-easy-virtual-handler *9b*
    (9b-channels-listing :uri "/channels") ()
  (with-output-to-string (*standard-output*)
    (mapc (lambda (channel-name)
            (princ channel-name)
            (princ "<br>"))
          (hash-table-keys (irc:channels nisp.i::*devel-bot*)))))