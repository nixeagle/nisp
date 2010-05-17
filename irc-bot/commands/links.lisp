(in-package :nisp.i)

(define-simple-command link
  (handler-case (network-tree::next-node)
    (error (condition)
      (network-tree::next-node
       (apply #'format nil "nil ~A:~A"
              (multiple-value-list (network-tree::first-command-word (remaining-parameters))))))))

(defmethod handle-command
    ((tree (eql "link")) (source connection) (user abstract-user)
     (address bot-channel) (identity abstract-identity)
     (action abstract-action) (content abstract-text-message-content))
  (unless #+nisp-vps (or (gethash "whbot-dev" (irc:users address) nil)
                         (gethash "nisp-devel" (irc:users address) nil)
                         (gethash "nisp-dev" (irc:users address) nil))
          #-nisp-vps nil
          (handler-case (nisp.network-tree::next-node)
            (error (condition)
              (declare (ignore condition))
              (nisp.network-tree::next-node
               (apply #'format nil "nil ~A:~A"
                      (multiple-value-list
                       (nisp.network-tree::first-command-word
                        (remaining-parameters)))))))))



(defun format-euler-problem-url-text (id)
  (let ((url (format nil "http://projecteuler.net/index.php?section=problems&id=~A" id)))
    (let ((it (dom:get-elements-by-tag-name (chtml:parse (drakma:http-request url) (rune-dom:make-dom-builder)) "p")))
      (format nil "Project Euler problem #~A: ~A <~A>"
              id
              (dom:data (dom:first-child (aref it (1- (fill-pointer it)))))
              (shorturl-is.gd url)))))

(define-simple-command link-euler
  (if (string-integer-p (remaining-parameters))
      (reply (format-euler-problem-url-text (remaining-parameters)))
      (reply  "Project Euler profile for ~A: ~A" (remaining-parameters)
              (shorturl-is.gd (format nil "http://projecteuler.net/index.php?section=profile&profile=~A" (remaining-parameters))))))

(defun format-link-wikipedia (params)
  (format nil "Wikipedia article ~A: ~A" params
          (format nil "http://en.wikipedia.org/wiki/~A"
                  (substitute #\_ #\Space params))))
(define-simple-command link-wikipedia
  (reply (format-link-wikipedia (remaining-parameters))))
(define-simple-command link-wiki
  (reply (format-link-wikipedia (remaining-parameters))))
(define-simple-command link-nil
  (reply (format-link-wikipedia (remaining-parameters))))

(iter (for x in-vector (dom:get-elements-by-tag-name
                        (chtml:parse (drakma:http-request "http://www.itl.nist.gov/div897/sqg/dads/ui.html")
                                     (rune-dom:make-dom-builder)) "a"))
      (when (search "HTML/" (dom:get-attribute x "href") :end2 5)
        (when (dom:text-node-p (dom:first-child x))
          (setf (gethash (dom:data (dom:first-child x)) *nist-compsci-dictionary*)
                (concatenate 'string "http://www.itl.nist.gov/div897/sqg/dads/"
                             (dom:get-attribute x "href"))))))

(define-simple-command link-nist
  (reply (gethash (remaining-parameters) *nist-compsci-dictionary*
                  (format nil "Requested page ~A does not exist in my index."
                          (remaining-parameters)))))

(define-simple-command link-cpan
  (reply "CPAN: http://search.cpan.org/search?mode=all&query=~A" (remaining-parameters)))

(define-simple-command link-emacs
  (reply "Emacwiki: ~A"
          (shorturl-is.gd (format nil "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=~A&sa=Search" (substitute #\+ #\Space (remaining-parameters))))))

(define-simple-command link-wk
  (reply "wiktionary: http://en.wiktionary.org/wiki/~A" (remaining-parameters)))

(defun format-link-wikihow (params &optional lang)
  (let ((article (substitute #\- #\Space params)))
    (format nil "wikiHow page ~A: ~A"
            (substitute #\Space #\- article)
            (if lang
                (format nil "http://~A.wikihow.com/~A"
                        lang article)
                (format nil "http://wikihow.com/~A"
                        article)))))

(define-simple-command link-wikihow
  (reply (format-link-wikihow (remaining-parameters))))

(define-simple-command link-google
  (reply "Google: http://google.com/search?q=~A"
         (substitute #\+ #\Space (remaining-parameters))))

(define-simple-command link-debpackage
  (reply "http://packages.debian.org/~A" (remaining-parameters)))


(define-simple-command link-rfc
  (if (string-integer-p (remaining-parameters))
      (reply "RFC: http://www.ietf.org/rfc/rfc~A" (remaining-parameters))
      (reply "RFCs are referred to by number")))