(in-package :nisp.9p)

(define-new-suite :nisp-eos-root)
(def-suite root :in :nisp-eos-root)

(test (9p-version-string-p :suite root)
  (with-fbound (9p-version-string-p)
    ("9P") t
    ("9PP") t
    ("P") nil
    (1) nil))

(HUNCHENTOOT:ACCEPTOR-SSL-P 3)