(defpackage #:php-serialization
  (:use :cl))

(defun ignore-reader (stream char)
  "Ignore the CHAR and move on!"
  (declare (ignore stream char))
  (values))

(deftype boolean-number ()
  "1 means t, 0 means nil."
  '(member #\1 #\0))

(defun assert-read-colon (stream)
  (assert (eql #\: (read-char stream t nil t))))

(defun read-through (char stream)
  (peek-char char stream t nil t)
  (read-char stream t nil t))

(defun |{-READER| (stream char)
  "Read {1 2 3} as (1 2 3)"
  (declare (ignore char))
  (labels ((group-assoc (list &optional alist)
             (declare (type list list))
             (if (consp list)
                 (group-assoc (cdr list)
                              (if (or (null alist) (consp (car alist)))
                                  (cons (car list) alist)
                                  (acons (car alist) (car list) (cdr alist))))
                 (reverse alist))))
    (group-assoc (read-delimited-list #\} stream))))

(defun object-reader (stream char)
  "Read a php object from STREAM."
  (let ((object-name (scalar-reader stream char)))
    (assert-read-colon stream)
    (read-through #\: stream)
    (list object-name (read stream t nil t))))

(defun scalar-reader (stream char)
  (declare (ignore char))
  (assert-read-colon stream)
  (read-through #\: stream)
  (read stream))

(defun boolean-reader (stream char)
  (declare (ignore char))
  (assert-read-colon stream)
  (let ((boolean-number (read-char stream)))
    (check-type boolean-number boolean-number)
    (eql #\1 boolean-number)))

(defun make-php-readtable ()
  (let ((*readtable* (copy-readtable)))
    (flet ((ignore (&rest chars)
             (mapcar (lambda (char)
                       (set-macro-character char #'ignore-reader))
                     chars)))
      (set-macro-character #\{ #'|{-READER| t)
      (set-macro-character #\} (get-macro-character #\) nil))
      (set-macro-character #\s #'scalar-reader)
      (set-macro-character #\a #'scalar-reader)
      (set-macro-character #\b #'boolean-reader)
      (set-macro-character #\O #'object-reader)
      (ignore #\: #\; #\i #\d)
      *readtable*)))

(defparameter *php-readtable* (make-php-readtable))
(defun php-readtable ()
  *php-readtable*)
(defun call-with-php-readtable (thunk)
  (let ((*readtable* (php-readtable)))
    (funcall thunk)))


(defun assert-print-unreadable (object)
  "Signals `print-not-readable' error when `*print-readably*' is true.

OBJECT is given to the :object parameter of `print-not-readable'"
  (when *print-readably*
    (error (make-condition 'print-not-readable
                           :object object))))

(defun pprint-string (stream string)
  "Print STRING to STREAM serialized for php."
  (assert-print-unreadable string)
  (let ((*print-pretty* nil))
    (format stream "s:~D:~S;" (length string) string)))

(defun pprint-list (stream list)
  "Print LIST to STREAM serialized as a php array."
  (assert-print-unreadable list)
  (let ((*print-pretty* nil))
    (format stream "a:~D:" (length list)))
  (format stream "{~{~S~}}" list))

(defun pprint-acons (stream acons)
  (assert-print-unreadable acons)
  (format stream "~S~S" (car acons) (cdr acons)))

(defun pprint-integer (stream integer)
  "Print INTEGER to STREAM serialized as a php integer."
  (assert-print-unreadable integer)
  (let ((*print-pretty* nil))
    (format stream "i:~D;" integer)))

(defun pprint-float (stream float)
  "Print FLOAT to STREAM serialized as a php decimal."
  (assert-print-unreadable float)
  (let ((*print-pretty* nil))
    (format stream "D:~D;" float)))

(defun pprint-boolean (stream boolean)
  (assert-print-unreadable boolean)
  (let ((*print-pretty* nil))
    (format stream "b:~D;" (if boolean 1 0))))

(defun php-array-element-p (object)
  "True if OBJECT can be serialized as a php array element.

PHP array elements always start with an integer or a string."
  (and (consp object)
       (or (integerp (car object))
           (stringp (car object)))))

(deftype php-array-element ()
  "The car of an acons must be an integer or string."
  '(and cons (satisfies php-array-element-p)))

(defun make-php-pprint-dispatch-table ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'string #'pprint-string)
    (set-pprint-dispatch 'integer #'pprint-integer)
    (set-pprint-dispatch 'float #'pprint-float)
    (set-pprint-dispatch 'boolean #'pprint-boolean)
    (set-pprint-dispatch 'php-array-element #'pprint-acons)
    (set-pprint-dispatch 'cons #'pprint-list)
    *print-pprint-dispatch*))

(defparameter *php-table* (make-php-pprint-dispatch-table)
  "Pretty print dispatch table for serializing to php.")

(defun php-pprint-table ()
  *php-table*)

(defun call-with-php-pprint-table (thunk)
  (declare (type function thunk))
  (let ((*print-pprint-dispatch* (php-pprint-table)))
    (funcall thunk)))

(defmacro with-php-pprint-table (&body body)
  `(call-with-php-pprint-table (lambda () ,@body)))

(eos:def-suite root)
(eos:test (unserialize-stream-array :suite root))

(eos:test (php-array-element-p :suite root)
  (eos:is-true (php-array-element-p '(1 . 2)))
  (eos:is-true (php-array-element-p '(1 . something)))
  (eos:is-true (php-array-element-p '("hi" . "hi")))
  (eos:is-false (php-array-element-p '(1.1 . 2)))
  (eos:is-false (php-array-element-p '(1 2)))
  (eos:is-false (php-array-element-p '("hi" 2))))
;;; END
