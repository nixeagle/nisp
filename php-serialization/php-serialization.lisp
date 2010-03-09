(defpackage #:php-serialization
  (:use :cl))

(defun ignore-reader (stream char)
  "Ignore the CHAR and move on!"
  (declare (ignore stream char))
  (values))

(deftype boolean-number ()
  "1 means t, 0 means nil."
  '(member #\1 #\0))

(flet ((assert-read-colon (stream)
         (assert (eql #\: (read-char stream t nil t))))
       (read-through (char stream)
         (peek-char char stream t nil t)
         (read-char stream t nil t)))

  (defun |{-READER| (stream char)
    "Read {1 2 3} as (1 2 3)"
    (declare (ignore char))
    (labels ((group-assoc (list &optional alist)
               (declare (type list list))
               (print alist)
               (if (consp list)
                   (group-assoc (cdr list)
                                (if (or (null alist) (consp (car alist)))
                                    (cons (car list) alist)
                                    (acons (car alist) (car list) (cdr alist))))
                   (reverse alist))))
      (group-assoc (read-delimited-list #\} stream t))))

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
    (read stream t nil t))

  (defun boolean-reader (stream char)
    (declare (ignore char))
    (assert-read-colon stream)
    (let ((boolean-number (read-char stream)))
      (check-type boolean-number boolean-number)
      (eql #\1 boolean-number))))

(defun test-read (string
                  &aux (stream (make-string-input-stream string)))
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
      (read stream))))

(eos:def-suite root)
(eos:test (unserialize-stream-array :suite root))
;;; END
