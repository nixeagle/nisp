(in-package :nisp.fbi.json-classes)

(define-new-suite :nisp-eos-root)

(def-suite root :in :nisp-eos-root)
(test (make-json-type-signature :suite root)
  (with-fbound (make-json-type-signature)
    ('((a . b) (c . d))) '(a c)))


(defmacro with-fbi-socket ((sock &optional
                                 (host "danopia.net")
                                 (port 5348)) &body body)
  `(let ((sock ',sock))
     (unwind-protect (progn
                       (setq sock (json-socket-connect ,host ,port))
                       ,@body)
       (close (socket-stream sock) :abort t))))

#+ ()
(with-fbi-socket (sock)
  (synchronous-json-request (make-instance 'auth
                                           :secret "sekrit"
                                           :user "Hi danopia from nixeagle!")
                            sock :json-symbols-package :nisp.fbi.json-classes))

#+ ()
(with-fbi-socket (sock)
  (synchronous-request (make-instance 'auth
                                      :secret "sekrit"
                                      :user "Hi danopia from nixeagle!")
                       sock))


#+ ()
(let ((sock (json-socket-connect "danopia.net" 5348)))
  ())