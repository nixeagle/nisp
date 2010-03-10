;;; These commands are not automatically loaded by the nisp system on
;;; purpose. These are all using packages that may not be loaded with the
;;; main nisp system and should be loaded manually until the packages they
;;; depend on are stable enough to load in with the nisp.i system.

(in-package :nisp.i)
(define-simple-command alpha
  (network-tree::next-node))
(define-simple-command alpha-unserialize
  (reply (list
          (php-serialization:with-php-readtable
            (read-from-string (remaining-parameters))))))
(define-simple-command alpha-serialize
  (let ((lisp-input (read-from-string (remaining-parameters))))
    (reply (php-serialization:with-php-pprint-table
             (princ-to-string lisp-input)))))


(define-simple-command emacs-uptime
  (reply (~::eval-in-emacs "(emacs-uptime)")))
(define-simple-command emacs-idletime
  (reply (~::eval-in-emacs "(format-seconds \"%Y, %D, %H, %M, %z%S\"
                 (float-time (or (current-idle-time) '(0 0 0))))")))

;;; END
