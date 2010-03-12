(in-package :nisp.i)

(define-simple-command beta
  (network-tree::next-node))


(defun shorturl-is.gd (string)
  (declare (type string string))
  (drakma:http-request "http://is.gd/api.php"
                       :parameters `(("longurl" . ,string))))

(define-simple-command beta-shorturl
  (reply (shorturl-is.gd (remaining-parameters))))