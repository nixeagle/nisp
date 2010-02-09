(defpackage #:nisp.fbi.json-classes
  (:use :cl :usocket :json :iterate :nisp.util-protocol :alexandria
        :nisp.util.json :eos :with-fbound)
  (:export :auth :subscribe :json-action-mixin
           :commit-author :commit-data
           :irc-data :irc-message :irc-private :publish
           :sender :url
           ;; methods
           #:json->alist #:make-json-type-signature
           #:json->string
           #:make-irc-private-message
           #:make-json-mixin-from-string
           #:make-subscribe #:json-nisp-message

           ;; publish
           #:action
           #:from
           ;; irc
           #:command
           #:args
           #:sender
           #:admin
           #:server
           #:channel
           ))

(in-package :nisp.fbi.json-classes)
(defparameter *fbi-json-signatures* (make-hash-table :test #'equal)
  "list -> type mapping")

(defun make-json-type-signature (alist)
  "Return a list for use as a type signature.

A type signature is basically a list of all keys in a hash table from cl-json"
  (mapcar #'car alist))

(defun make-json-mixin-from-string (string)
  ;; Not positive if this needs to stay or not...
  (declare (type string string))
  (let ((json:*prototype-name* 'hash-type)
        (json:*json-symbols-package* :nisp.fbi.json-classes))
    (json:with-decoder-simple-clos-semantics
      (json:decode-json-from-string
       string))))

(defun find-json-type (bindings)
  (let ((type-class (gethash (make-json-type-signature bindings)
                             *fbi-json-signatures* nil)))
    (if type-class                ;nil for no signature by this binding.
        (if (functionp type-class)
            (funcall type-class bindings)
            type-class)
        (progn
          (format t
                  "FBI type signature error: ~A ~
                    Quickfix: (defclass <classnamehere> (json-mixin) ~A)"
                  (make-json-type-signature bindings)
                  (string-downcase
                   (princ-to-string (make-json-type-signature bindings))))
          nil))))


(defgeneric action (object))
(defgeneric from (object)
  (:documentation "From is always related to who on FBI sent a message."))
(defclass json-action-mixin (json-mixin)
  ((action :initform (error "~&Action cannot be left empty! ~
                             ~&FBI fails silently if this is not provided.")
           :type string
           :accessor action))
  (:documentation "All classes representing FBI actions should inherit this."))

(defclass irc-data-mixin () ()
  (:documentation "Anything with command info should superclass this."))

(defclass auth (json-action-mixin)
  ((action :initform "auth")
   (user :accessor user
         :initarg :user)
   (secret :accessor secret
           :initarg :secret))
  (:documentation "Auth with FBI."))

(defclass subscribe (json-action-mixin)
  ((action :initform "subscribe")
   (channels :accessor channels
             :initarg :channels))
  (:documentation "Subscribe to FBI channels."))

(defclass publish (json-action-mixin irc-data-mixin)
  ((from :accessor from) (action :initform "publish") channel
        (data :accessor data)))

(defclass author (json-mixin)
  (name email))
(defclass commit-author (author) ()
  (:documentation "Author of a commit from github."))

(defclass sender (json-mixin) (nick ident host))

(defgeneric long-url (object))
(defgeneric short-url (object))
(defgeneric url (object)
  ;; We do this by first looking for short-url, if its unbound or does
  ;; not exist go looking for long-url.
  (:documentation "Get the shortest url possible."))

(defclass url ()
  ((url :type string
        :initarg :long-url
        :accessor long-url)
   (shorturl :type string
             :initarg :short-url
             :accessor short-url))
  (:documentation "Represents a url in FBI data. The server always gets ~
  a tiny url if possible, but if not we will be prepared to always get ~
  _a_ url."))

(defmethod url ((url url))
  (if (slot-boundp url 'shorturl)
      (short-url url)
      (long-url url)))
(defmethod url ((object publish))
  (url (data object)))


(defgeneric command (object)
  (:method ((object irc-data-mixin))
    (command object)))
(defgeneric args (object)
  (:method ((object irc-data-mixin))
    (args object)))
(defgeneric sender (object)
  (:method ((object irc-data-mixin))
    (sender object)))
(defgeneric admin (object)
  (:method ((object irc-data-mixin))
    (admin object)))
(defgeneric server (object)
  (:method ((object irc-data-mixin))
    (server object)))
(defgeneric channel (object)
  (:method ((object irc-data-mixin))
    (channel object)))

(defclass irc-data (irc-data-mixin json-mixin)
  ((command :accessor command)
   (args :accessor args)
   (sender :accessor sender)
   (admin :accessor admin)
   (server :accessor server)
   (channel :accessor channel)
   (default--project :accessor default-project)))

(macrolet ((publish-irc-data (name)
             `(defmethod ,name ((object publish))
               (,name (data object)))))
    (publish-irc-data command)
    (publish-irc-data args)
    (publish-irc-data admin)
    (publish-irc-data server)
    (publish-irc-data channel))

(defclass commit-data (url json-mixin)
  (message commit project project-2
           (author :accessor author) branch))
(defclass irc-message (json-mixin)
  ((id :initarg :id)
   (server :initarg :server)
   (channel :initarg :channel)
   (message :initarg :message)))
(defclass irc-private (json-mixin)
  ((to :initarg :to)
   (action :initform "private")
   (data :initarg :data
         :type irc-message)))

(defun make-irc-private-message (to irc-server irc-channel irc-message)
  (declare (type string to irc-channel irc-message)
           (type positive-fixnum irc-server))
  (make-instance 'irc-private :to to
                 :data (make-instance 'irc-message
                                      :message irc-message
                                      :channel irc-channel
                                      :server irc-server
                                      :id nil)))

(macrolet ((define-describe-object (object slot)
             `(defmethod describe-object ((s ,object) stream)
                (call-next-method)
                (terpri stream)
                (describe (,slot s) stream))))
  (define-describe-object publish data)
  (define-describe-object commit-data author)
  (define-describe-object irc-private data))

(macrolet ((define-signature (key name)
             `(setf (gethash ',key *fbi-json-signatures*)
                    (if (listp ',name) ,name ',name))))
  (clrhash *fbi-json-signatures*)
  (define-signature (name email) 'commit-author)
  (define-signature (message commit project project-2 author url branch shorturl)
      'commit-data)
  (define-signature (user action) 'auth)
  (define-signature (nick ident host) 'sender)
  (define-signature (command args sender admin server channel default--project)
      'irc-data)
  (define-signature (action channels) 'subscribe)
  (define-signature (from action channel data)
      (lambda (bindings) (intern (nstring-upcase (cdr (assoc 'action bindings)))
                                 :nisp.fbi.json-classes))))

(defgeneric json->alist (object))
(defmethod json->alist ((object json-mixin))
  (decode-json-from-string (encode-json-to-string object)))
(defmethod json->alist ((object string))
  (decode-json-from-string object))
(defmethod json->alist ((object stream))
  (decode-json object))

(defgeneric json->string (object))
(defmethod json->string ((object json-mixin))
  (encode-json-to-string object))

#+ ()
(defmethod make-object :around (bindings (symbol symbol) &optional superclasses)
  (if (or (null symbol) (find-class symbol nil))
      (call-next-method)
      (make-object bindings nil superclasses)))
#+ ()
(defmethod make-object :around (bindings (symbol (eql nil)) &optional superclasses)
  (let ((type-class (nisp.fbi.json-classes::find-json-type bindings)))
    (let ((class (find-class type-class nil))
          (signature (make-json-type-signature bindings)))
      (format t "~&~16A . ~A~%" type-class signature)
      (when (and (null class) type-class)
        (nisp.fbi.json-classes::json-nisp-message
         (format nil "Class ~A does not exist. Bindings were: ~A"
                 type-class signature)))
      (if (and (null symbol) (null class)) 
          (call-next-method bindings nil 
                            superclasses)
          (make-object bindings type-class superclasses)))))