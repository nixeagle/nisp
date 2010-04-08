(in-package :nisp.9p)


(def-suite root)

(test (9p-version-string-p :suite root)
  #+ ()
  (with-fbound (9p-version-string-p)
    ("9P") t
    ("9PP") t
    ("P") nil
    (1) nil))

(HUNCHENTOOT:ACCEPTOR-SSL-P 3)