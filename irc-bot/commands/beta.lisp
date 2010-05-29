(in-package :nisp.i)

(define-simple-command beta
  (network-tree::next-node))


(defun shorturl-is.gd (string)
  (declare (type string string))
  (drakma:http-request "http://is.gd/api.php"
                       :parameters `(("longurl" . ,string))))

(define-simple-command beta-shorturl
  (reply (shorturl-is.gd (remaining-parameters))))

(define-simple-command beta-getip
  (reply (format nil "~{~{~A~^.~}~^ ~}"
                 (mapcar (lambda (ip-vector)
                           (coerce ip-vector 'list))
                         (usocket::get-hosts-by-name (remaining-parameters))))))
