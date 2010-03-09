;;; These commands are not automatically loaded by the nisp system on
;;; purpose. These are all using packages that may not be loaded with the
;;; main nisp system and should be loaded manually until the packages they
;;; depend on are stable enough to load in with the nisp.i system.

(in-package :nisp.i)
(define-simple-command alpha
  (network-tree::next-node))
(define-simple-command alpha-unserialize
  (reply (princ-to-string (php-serialization::test-read
                           (remaining-parameters)))))