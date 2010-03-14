(in-package :ppjs)

(defmacro js (&body body)
  `(call-with-js-pprint-table (lambda () (prin1-to-string ',@body))))

(defmacro js= (string &body js-body)
  (typecase string
    `(is (string= ,string (js ,@js-body)))))

(defvar *test-list*)
(defmacro jst (&body body)
  `(let ((*print-pretty* nil))
     (prin1
      (list 'js= (call-with-js-pprint-table (lambda () (let ((*print-pretty* t)) (prin1-to-string ',@body))))
            ',@body))))

(defmacro jsti (&body body)
  `(progn
    (irc:privmsg nisp.i::*devel-bot* "#bots"
                  (format nil "js: ~A // ~A => ~S"
                          (remove #\newline (call-with-js-pprint-table (lambda () (prin1-to-string ',@body))))
                          (prin1-to-string ',@body)
                          ,@body))
    (let ((*print-pretty* nil))
       (prin1
        (list 'js=
              (call-with-js-pprint-table (lambda () (let ((*print-pretty* t)) (prin1-to-string ',@body))))
              ',@body))))
  )
(def-suite root
    :description "Tests for pretty printing javascript library.")
(in-suite root)
(test pprint-+
  (js= "1 + 2;" (+ 1 2))
  (js= "1;" (+ 1))
  (js= "0;" (+))
  (js= "1 + (2 + 3);" (+ 1 (+ 2 3))))

(test pprint--
  (js= "-1;" (- 1))
  (js= "1 - 3;" (- 1 3))
  (signals error (js (-))))

(test pprint-*
  (js= "1;" (* 1))
  (js= "1 * 2;" (* 1 2))
  (js= "1;" (*)))

(test pprint-/
  (JS= "1/2;" (/ 2))
  (JS= "1/1;" (/ 1))
  (signals DIVISION-BY-ZERO
    (js (/ 0)))
  (JS= "1 / 2;" (/ 1 2))
  (JS= "1 / 2 / 3;" (/ 1 2 3))
  (signals error
    (js (/))))

(test pprint-//nesting
  (JS= "1 / (2 + 4) / 3;" (/ 1 (+ 2 4) 3)))