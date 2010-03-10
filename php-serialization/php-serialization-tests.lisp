(in-package :php-serialization)
(eval-when (:compile-toplevel :load-toplevel)
  (use-package (list :eos)))

(def-suite root)
(test (unserialize-stream-array :suite root))

(test (assert-read-colon :suite root)
  (finishes (assert-read-colon (make-string-input-stream ":")))
  (signals error (assert-read-colon (make-string-input-stream "1")))
  (signals error (assert-read-colon (make-string-input-stream ""))))

(test (read-through :suite root)
  (is (eql #\a (read-through #\a (make-string-input-stream "   a  ")))))

(test (scalar-reader :suite root)
  (flet ((r (string)
           (scalar-reader (make-string-input-stream string) #\s)))
    (is (string= "hi" (r ":2:\"hi\";")))
    (is (string= "" (r ":0:\"\";")))))

(test (boolean-reader :suite root)
  (flet ((r (string)
           (boolean-reader (make-string-input-stream string) #\b)))
    (is (eq t (r ":1;")))
    (is (eq nil (r ":0;")))
    (signals error (r ":9;"))))

(test (|{-READER| :suite root)
  "Make sure we can read an array."
  (with-php-readtable                   ;No easy way to test {-READER alone
    (is (equal `((0 . 2) (1 . 4)) (read-from-string "a:2:{i:0;i:2;i:1;i:4;}")))
    (is (equal `((0 . 2) (1 . 4) (2 . ((0 . 2) (1 . 4))))
               (read-from-string "a:3:{i:0;i:2;i:1;i:4;i:2;a:2:{i:0;i:2;i:1;i:4;}}")))))

;;; Printer
(test (php-array-element-p :suite root)
  (is-true (php-array-element-p '(1 . 2)))
  (is-true (php-array-element-p '(1 . something)))
  (is-true (php-array-element-p '("hi" . "hi")))
  (is-false (php-array-element-p '(1.1 . 2)))
  (is-true (php-array-element-p '(1 2)))
  (is-true (php-array-element-p '("hi" 2))))

(test (pprint-boolean :suite root)
  (flet ((pp (boolean)
           (with-output-to-string (stream)
             (pprint-boolean stream boolean))))
    (is (string= "b:1;" (pp t)))
    (is (string= "b:0;" (pp nil)))))

(test (pprint-float :suite root)
  (flet ((pp (float)
           (with-output-to-string (stream)
             (pprint-float stream float))))
    (is (string= "D:1.1;" (pp 1.1)))
    (is (string= "D:101.10011;" (pp 101.10011)))
    (is (string= "D:0.1;" (pp 0.1)))))

(test (pprint-integer :suite root)
  (flet ((pp (integer)
           (with-output-to-string (stream)
             (pprint-integer stream integer))))
    (is (string= "i:0;" (pp 0)))
    (is (string= "i:-1;" (pp -1)))
    (is (string= "i:1;" (pp 1)))))

(test (pprint-string :suite root)
  (flet ((pp (string)
           (with-output-to-string (stream)
             (pprint-string stream string))))
    (is (string= "s:4:\"four\";" (pp "four")))
    (is (string= "s:0:\"\";" (pp "")))))

(test (pprint-acons :suite root)
  (flet ((pp (acons)
           (with-php-pprint-table
             (with-output-to-string (stream)
               (pprint-acons stream acons)))))
    (is (string= "i:1;i:1;" (pp (cons 1 1))))
    (is (string= "s:3:\"key\";s:5:\"value\";"
                 (pp (cons "key" "value"))))
    (is (string= "i:1;s:5:\"value\";"
                 (pp (cons 1 "value"))))))

(test (pprint-list :suite root)
  (flet ((pp (alist)
           (with-php-pprint-table
             (with-output-to-string (stream)
               (pprint-list stream alist)))))
    (is (string= "a:1:{i:1;s:5:\"value\";}"(pp (acons 1 "value" ()))))))

(test (assert-print-unreadable :suite root)
  (let ((*print-readably* t))
    (signals print-not-readable (assert-print-unreadable t)))
  (finishes (assert-print-unreadable t)))

;;; END
