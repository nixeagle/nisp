(in-package :nisp.util.declarations)

(with-fbound:define-new-suite :nisp-eos-root)
(def-suite root :in :nisp-eos-root)
(test (get-safety-setting :suite root)
  "Test all 4 valid (optimize (safety)) values."
  (iter (for n :from 0 :to 3)
        (locally (proclaim `(optimize (safety ,n)))
          (is (= n (get-safety-setting))))))


;;;{{{ palindrome
(in-package :nisp.util.palindrome)
(eos:def-suite root :in :nisp-eos-root)
(eos:test (palindromep :suite root)
  (with-fbound:with-fbound (palindromep)
    (11) t
    (12) nil
    ("aba") t
    (14541) t
    ("") t))

;;;}}}