(in-package :nisp.i-zeta)

;;;{{{ Test generic function <new funcallable type>
(defclass test-generic-function (standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'test-method)))

(defclass test-method (standard-method) ())

(defun get-method-lambda-forms (lambda-expression)
  "Get LAMBDA-EXPRESSION in `make-method-lambda'."
  ;; This is probably very specific to my test-generic-function
  (cddddr lambda-expression))

;;; Now I have to override compute applicatable methods
(defmethod closer-mop:compute-applicable-methods-using-classes
    ((generic-function test-generic-function) classes)
  "For now just print information.~%"
  (call-next-method))

(defmethod compute-applicable-methods
    ((generic-function test-generic-function) args)
  "For now just print information."
  (call-next-method))

(defmethod closer-mop:compute-effective-method
    ((generic-function test-generic-function)
     method-combination
     applicable-methods)
  "Pass APPLICABLE-METHODS twice for use in defining THIS-METHOD."
  `(call-method ,(car applicable-methods) ,(cdr applicable-methods)
                ,(car applicable-methods)))

(defmethod closer-mop:make-method-lambda
    ((generic-function test-generic-function) method expression environment)
  (declare (ignore environment))
  `(lambda (args next-methods this-method)
     (,(call-next-method
        generic-function method
        `(lambda ,(cadr expression)
           (labels ((this-method () this-method)
                    (call-next-method (&rest new-args)
                      (when (= 3 (nisp.util.declarations:get-safety-setting))
                        (assert (nisp.mop:compare-generic-applicable-methods
                                 (or new-args args) args
                                 (closer-mop:method-generic-function this-method))
                                nil 'nisp.mop::applicable-methods-mismatch-error
                                :old-args args
                                :new-args new-args
                                :generic-function
                                (closer-mop:method-generic-function this-method)))
                      (format t "cnm: ~S ::NA: ~S" args new-args)
                      (funcall (closer-mop:method-function (car next-methods))
                               (or new-args args)
                               (cdr next-methods)
                               (car next-methods)))
                    (next-method-p () (not (null next-methods))))
             (declare (ignorable (function next-method-p)
                                 (function this-method)))
             ,@(cddr expression)))
        environment)
       args next-methods)))

#+ ()`(lambda (args next-methods this-method)
     (,(call-next-method gf method
         `(lambda ,(cadr lambda-expression)
            (flet ((this-method () this-method)
                   (call-next-method (&rest cnm-args)
                     (funcall (method-function (car next-methods))
                              (or cnm-args args)
                              (cdr next-methods)
                              (car next-methods)))
                   (next-method-p ()
                     (not (null next-methods))))
              ,@(cddr lambda-expression)))
          environment)
       args next-methods))

;;;}}}

(in-package :nisp.i)

;;;{{{ command funcallable class
(defclass command-generic-function (standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'command-method))
  (:documentation "Used for handling messages."))

(defclass command-method (standard-method) ())

(defclass command-specializer (closer-mop:specializer)
  ((direct-nodes :initform (make-hash-table :test 'eq :weakness :value)
                 :reader command-specializer-direct-nodes)
   (object :reader command-specializer-object
           :reader class-name :initarg :command)
   (direct-methods :initform nil
                   :reader closer-mop:specializer-direct-methods)))

(defmethod sb-pcl::specializer-type ((specializer command-specializer))
  ;; Fixes arglist printing:
  ;; instead of NIL we get (EQL <something>).
  `(eql ,@(command-specializer-object specializer)))

(defmethod closer-mop:add-direct-method ((specializer command-specializer)
                                         method)
  (pushnew method (slot-value specializer 'direct-methods)))

(defmethod closer-mop:remove-direct-method ((specializer command-specializer)
                                            method)
  (deletef (slot-value specializer 'direct-methods) method))

(defgeneric intern-command-specializer (command)
  (:documentation "Intern COMMAND into the command obarray."))

(defgeneric ensure-command-symbol (symbol))
(defmethod ensure-command-symbol ((symbol symbol))
  (ensure-symbol symbol :nisp.i.command-argument-symbols))
(defmethod ensure-command-symbol ((symbols cons))
  (mapcar #'ensure-command-symbol symbols))

(defmethod intern-command-specializer ((symbol symbol))
  (or (find-command-specializer (ensure-command-symbol symbol))
      (%add-command-specializer (get-command-specializer-obarray)
                                (ensure-command-symbol symbol)
                                (list (ensure-command-symbol symbol)))))

(defmethod intern-command-specializer ((symbols cons))
  (iter (with hash = (get-command-specializer-obarray))
        (for symbol :in symbols)
        (collect (ensure-command-symbol symbol) :into sofar)
        (setq hash
              (or (%get-command-specializer hash (ensure-command-symbol symbol))
                  (%add-command-specializer hash (ensure-command-symbol symbol) sofar)))
        (finally (return hash))))

(defvar *command-specializer-obarray*)
;;; Hash for interning specializer objects.
;;; This is a weak hash on the values to avoid the same bug that sbcl
;;; currently has with eql specializers.
(let ((obarray (or (when (boundp '*command-specializer-obarray*)
                     *command-specializer-obarray*)
                   (setq *command-specializer-obarray*
                         (make-hash-table :test 'eq :weakness :value)))))
  (defmethod %get-command-specializer ((hash hash-table)
                                       (symbol symbol))
    (gethash symbol hash))
  (defmethod %get-command-specializer ((specializer command-specializer)
                                       (symbol symbol))
    (%get-command-specializer (command-specializer-direct-nodes specializer)
                              symbol))
  (defmethod %get-command-specializer (no-hash (symbol symbol))
    "If we are passed no hash, we have no choice but to return nil."
    nil)

  (defmethod find-command-specializer ((symbol symbol)
                                       &optional (hash obarray))
    (%get-command-specializer hash (ensure-command-symbol symbol)))
  (defmethod find-command-specializer ((symbols cons)
                                       &optional
                                       (obarray obarray))
    (reduce #'%get-command-specializer (ensure-command-symbol symbols)
            :initial-value obarray))
  (defmethod %add-command-specializer ((hash hash-table)
                                       (symbol symbol)
                                       (symbols list))
    (setf (gethash symbol hash)
          (make-instance 'command-specializer
                         :command (ensure-command-symbol symbols))))
  (defmethod %add-command-specializer ((specializer command-specializer)
                                       (symbol symbol)
                                       (symbols list))
    (%add-command-specializer (command-specializer-direct-nodes specializer)
                              symbol
                              (ensure-command-symbol symbols)))
  (defmethod %remove-command-specializer ((hash hash-table) (symbol symbol))
    (remhash symbol hash))
  (defmethod %remove-command-specializer ((specializer command-specializer)
                                          (symbol symbol))
    (%remove-command-specializer (command-specializer-direct-nodes specializer)
                                 symbol))

  (defun clear-command-specializer-obarray ()
    "Delete every entry in command obarray.

Be warned that this may not be the brightest idea to do with an obarray
that has a ton of references by defined commands."
    (clrhash obarray))
  (defun get-command-specializer-obarray ()
    "Return the hashtable of command specializers."
    obarray))

(defun maybe-intern-command-specializer (input)
  (if (and (consp input) (eq (car input) 'command))
      (intern-command-specializer (cdr input))
      input))
(defun transform-method-specializers (input)
  (mapcar #'maybe-intern-command-specializer input))
;;; Now we need to make a specializer class. Most of this is from sbcl's
;;; boot.lisp `real-make-method-specializers-form'.
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function command-generic-function)
     method specializer-names environment)
  (let ((names (transform-method-specializers specializer-names)))
    (call-next-method generic-function method names environment)))

(defmethod compute-applicable-methods ((generic-function command-generic-function)
                                       args)
  ;; Command functions always have as the first argument a command-specializer.
  (let ((command-specializer (find-command-specializer (car args))))
    (when command-specializer
      (irc:privmsg
       *bot* "#bots"
       (remove-newlines
        (format nil "Debugging compute-applicable-methods: ~A"
                (iter (for method :in (generic-function-methods generic-function))
                      (when (eq command-specializer
                                (car (method-specializers method)))
                        (collect method))))))))
  (call-next-method))

(defmethod compute-discriminating-function
    ((generic-function command-generic-function))
  (let ((it (call-next-method)))
    (describe it)
    it))

(defmethod compute-applicable-methods-using-classes
    ((generic-function command-generic-function) args)
  "No cache for `command-generic-function' for now."
  (values args nil))



(defmethod compute-effective-method
    ((generic-function command-generic-function)
     method-combination applicable-methods)
  #+ () (lambda (a b c d e) (list a b c d e))
  (call-next-method))

(defmethod make-method-lambda
    ((generic-function command-generic-function) method expression environment)
  (declare (ignore environment))
  (call-next-method))

(defmethod make-load-form ((self command-specializer) &optional env)
  (declare (ignore env))

  `(intern-command-specializer ',(command-specializer-object self)))

;;;}}}
;;;END