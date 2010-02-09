(in-package :nisp.fbi.json-classes)

(def-suite root)
(test (make-json-type-signature :suite root)
  (with-fbound (make-json-type-signature)
    ('((a . b) (c . d))) '(a c)))

