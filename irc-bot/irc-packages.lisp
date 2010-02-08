(in-package :nisp.i)

(defvar *irc-package-prefix* :nisp.i.packages.
  "Prefix that is always appended to IRC packages.")

(defun normalize-network-name (network-name)
  "Replace all periods in NETWORK-NAME with dashes."
  (declare (type string network-name))
  (string-upcase (substitute #\- #\. network-name)))

(defun %ensure-network-package (package &key (prefix *irc-package-prefix*))
  "Add PREFIX to PACKAGE."
   (declare (type (or null string symbol) prefix)
            (type string package))
   (make-empty-package
    (concatenate 'string
                 (typecase prefix
                   (string prefix)
                   (symbol (if prefix
                               (symbol-name prefix)))) 
                 (normalize-network-name package)) :ignore))

(defgeneric ensure-network-package (package &key prefix &allow-other-keys)
  (:documentation "Make sure PACKAGE exists."))
(defmethod ensure-network-package ((package string) &key prefix)
  "Concat PREFIX to PACKAGE if it exists."
  (%ensure-network-package package :prefix (or prefix *irc-package-prefix*)))
(defmethod ensure-network-package ((package symbol) &key prefix)
  (%ensure-network-package (symbol-name package)
                           :prefix (or prefix *irc-package-prefix*)))


#+ () (format-symbol "irc.eighthbit.net" "it")