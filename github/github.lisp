(in-package :nisp.github-system)

(defpackage #:nisp.github
  (:use :cl :json :iterate))

(in-package :nisp.github)

(defparameter +github-api-url+ "https://github.com/api/v2/json"
  ;; Use only the json interface, we do not want to implement the xml or
  ;; yaml interfaces.
  "Github api location.
This is the same for every call.")

(pushnew (cons "application" "json") drakma:*text-content-types*)

(defvar *current-prototype* nil
  "Stores the key of an object until its stored in `*PREVIOUS-PROTOTYPE*'.")
(defvar *previous-prototype* nil
  "Stores the prototype of the json class above the current one.

For example: {\"user\":{\"plan\":{\"name\":....}}}
When parsing the plan json object, this will be set to \"USER\".")

(defun github-request (&rest parameters)
  "Ask github about PARAMETERS."
  (with-github-content-types
    (drakma:http-request (apply #'build-github-api-url parameters)
                         :want-stream t)))

(defun github-request->alist (&rest parameters)
  "Ask github about PARAMETERS and return them as an alist."
  (let ((result (apply #'github-request parameters)))
    (prog1 (with-decoder-simple-list-semantics
             (let ((json:*json-symbols-package* :nisp.github))
               (decode-json result))) 
      (close result))))

(defmacro with-github-content-types (&body body)
  "Evaluate BODY treating application/json as text."
  `(let ((drakma:*text-content-types* '(("application" . "json")
                                        ("text" . nil))))
     ,@body))

(defun github-authed-request (login token &rest parameters)
  (with-github-content-types
    (drakma:http-request (apply #'build-github-api-url parameters)
                         :method :post
                         :want-stream t
                         :parameters
                         `(,(and login `("login" . ,login))
                            ,(and token `("token" . ,token))))))

(defun set-prototype (key)
  "Make KEY the json `*PROTOTYPE*'."
  (setq json::*prototype* key))

(defun beginning-of-object ()
  "Do more at prototype init"
  (setq *previous-prototype* *current-prototype*) (setq *current-prototype* nil)
  (json::init-accumulator-and-prototype))

(defun camel-case-to-lisp (string)
  (declare (type string string))
  (string-upcase (iter (for char :in-string string)
                       (if (char= #\_ char)
                           (collect #\- :result-type string)
                           (collect char :result-type string)))))

(defun key-add-or-set (key)
  "Mark KEY a prototype if it is, and add it to the accumulator."
  (let ((key (funcall #'camel-case-to-lisp key)))
    (print key)
    (if (and (not *current-prototype*)
             (or (string= key "USER")
                 (string= key "PLAN")
                 (string= key "USERS")))
        (progn (setq json::*accumulator-last*
                     (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil)))
               (setq *current-prototype* key)
              #+ () (pushnew (cons "PROTOTYPE" key) (cddr json::*accumulator*))
               (set-prototype t))
        (setq json::*accumulator-last*
              (setf (cdr json::*accumulator-last*) (cons (cons key nil) nil))))
    json::*accumulator*))


(defun value-add-or-set (value)
  "If VALUE (in a JSON Object being decoded) corresponds to a key which
matches *PROTOTYPE-NAME*, set VALUE to be the prototype of the Object.
Otherwise, do the same as ACCUMULATOR-ADD-VALUE."
  (if (eql json::*prototype* t)
      (progn
        (check-type value (or json::prototype string)
                    (format nil "Invalid prototype: ~S." value))
        (setq json::*prototype* *current-prototype*)
        json::*accumulator*)
      (json::accumulator-add-value value)))

;;; Modified from cl-json 
(defun accumulator-get-object ()
  "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively.  If the
JSON Object had a prototype field infer the class of the object and
the package wherein to intern slot names from the prototype.
Otherwise, create a FLUID-OBJECT with slots interned in
*JSON-SYMBOLS-PACKAGE*."
  (flet ((as-symbol (value)
           (etypecase value
             (string (intern value))
             (symbol value)))
         (intern-keys (bindings)
           (loop for (key . value) in bindings
              collect (cons (json:json-intern key) value))))
    (if (typep *previous-prototype* 'json::prototype)
        (with-slots (lisp-class lisp-superclasses lisp-package)
            *previous-prototype*
          (let* ((package-name (as-symbol lisp-package))
                 (json:*json-symbols-package*
                  (if package-name
                      (or (find-package package-name)
                          (error 'package-error :package package-name))
                      json::*json-symbols-package*))
                 (class (as-symbol lisp-class))
                 (superclasses (mapcar #'as-symbol lisp-superclasses)))
            (json::maybe-add-prototype
             (json:make-object (intern-keys (cdr json::*accumulator*))
                          class superclasses)
             *previous-prototype*)))
        (let ((bindings (intern-keys (cdr json::*accumulator*)))
              (class (if (stringp *previous-prototype*) (as-symbol *previous-prototype*))))
          (when (and *previous-prototype* (not class))
            (push (cons json::*prototype-name* *previous-prototype*) bindings))
          (print class)
          (json:make-object bindings class)))))

(defmacro with-github-decoder (&body body)
  "Execute BODY with decoder bindings appropriate for github's api."
  `(json:bind-custom-vars
      (:beginning-of-object #'beginning-of-object 
                            :object-key #'key-add-or-set 
                            :object-value #'value-add-or-set
                            :end-of-object #'accumulator-get-object 
                            :object-scope '(json:*INTERNAL-DECODER*
                                            *current-prototype*
                                            *previous-prototype*))
     ,@body))

(defgeneric to-json (object)
  (:method :around (obj)
           (let ((json:*json-symbols-package* :nisp.github))
             (with-local-class-registry (:inherit nil)
               (call-next-method)))))
(defmethod to-json ((obj string))
  (with-github-decoder 
    (json:decode-json-from-string obj)))
(defmethod to-json ((obj stream))
  "Read directly from a stream and close the stream when done."
  (prog1 (with-github-decoder
           (json:decode-json obj))
    (close obj)))

(defgeneric json->class (object class)
  (:documentation "Store json in OBJECT to CLASS"))

(defmethod json->class ((object stream)
                        (class symbol))
  "Store json from STREAM in an instance of CLASS."
  (make-object (with-decoder-simple-list-semantics
                 (decode-json object))
               class))

(defmethod json->class :around (object class)
  "Set package to nisp.github and use local class registry."
  (let ((json:*json-symbols-package* :nisp.github))
    (with-local-class-registry (:inherit nil)
      (call-next-method))))

(defmethod json->class :after ((object stream) class)
  "Close STREAM after we are done with it."
  (close object))

;;; JSON classes
(defclass user ()
  (plan gravatar-id name company location created-at
        collaborators disk-usage
        public-gist-count public-repo-count
        blog following-count id private-gist-count
        owned-private-repo-count total-private-repo-count
        followers-count login email))

(defclass plan ()
  (name collaborators space private-repos))

(defclass users ()
  (name location followers username language fullname
        repos id type pushed score created)
  (:documentation "Describes a github user search result."))

(defclass followers ()
    (users)
  (:documentation "List of users who follow someone."))

;;; utils
(defun build-github-api-url (&rest parameters)
  "Build a request url using PARAMETERS."
  (reduce (lambda (prior new)
            (concatenate 'string prior "/" new))
          (cons +github-api-url+ parameters)))

(defmethod make-object :before (bindings
                                (class (eql nil))
                                &optional superclasses)
  "Debug helper to print the keys of BINDINGS."
  (write (mapcar #'car bindings)
         :case :downcase))

;;; API calls
(defun show-user (user)
  (slot-value (to-json (github-request "user" "show" user)) 'user))

(defun show-followers (username)
  "List all followers of USERNAME."
  (declare (type string username))
  (json->class (github-request "user" "show" username "followers")
               'followers))

(defun search-users (username)
  "Search github for USERNAME."
  (declare (type string username))
  (slot-value (to-json (github-request "user" "search" username))
              'users))

;;; End file
