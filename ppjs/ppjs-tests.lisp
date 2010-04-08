(in-package :ppjs)

(defmacro js (&body body)
  `(call-with-js-pprint-table (lambda () (prin1-to-string ',@body))))

(defmacro js= (string &body js-body)
  `(is (string= ,(concatenate 'string string ";
")
                (js ,@js-body))))



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
  (js= "1 + 2" (+ 1 2))
  (js= "1" (+ 1))
  (js= "0" (+))
  (js= "1 + (2 + 3)" (+ 1 (+ 2 3))))

(test pprint--
  (js= "-1" (- 1))
  (js= "1 - 3" (- 1 3))
  (js= "1" (- -1))
  (signals argument-count-error (js (-))))

(test pprint-*
  (js= "1" (* 1))
  (js= "1 * 2" (* 1 2))
  (js= "1" (*)))

(test pprint-/
  (JS= "1/2" (/ 2))
  (JS= "1" (/ 1))
  (signals DIVISION-BY-ZERO
    (js (/ 0)))
  (JS= "1 / 2" (/ 1 2))
  (JS= "1 / 2 / 3" (/ 1 2 3))
  (signals error
    (js (/))))

(test pprint-//nesting
  (JS= "1 / (2 + 4) / 3" (/ 1 (+ 2 4) 3)))

(test pprint-defun/hairy
  (JS= "function jstest (a b) {
    return a / b / c / d / e / f / g / h / i / j / k / l / m / n / o / p / q
             / r / s / t / u / v / w / x / y / z / a / b / c / d / e / f / g
             / h / i / j / k / l / m / n / o / p / q / r / s / t / u / v / w
             / x / y / z / a / b / c / d
             / (a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p
                  + q + r + s + t + u + v + w + x + y + z + a + b + c + d + e
                  + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t
                  + u + v + w + x + y + z + a + b + c + d + e + f + g + h + i
                  + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x
                  + y + z + a + b + c + d + e + f + g + h + i + j + k + l + m
                  + n + o + p + q + r + s + t + u + v + w + x
                  + function jstestinner () {
                        return a * b * c * d * e * f * g * h * i * j * k * l
                                 * m * n * o * p * q * r * s * t * u * v * w
                                 * x * y * z * a * b * c * d * e * f * g * h
                                 * i * j * k * l * m * n * o * p * q * r * s
                                 * t * u * v * w * x * y * z * a * b * c * d
                                 * e * f * g * h * i * j * k * l * m * n * o
                                 * p * q * r * s * t * u * v * w * x * y * z
                                 * a * b * c * d * e * f * g * h * i * j * k
                                 * l * m * n * o * p * q * r * s * t * u * v
                                 * w * x * y * z;
                    }
                  + y + z)
             / e / f / g / h / i / j / k / l / m / n / o / p / q / r / s / t
             / u / v / w / x / y / z / a / b / c / d / e / f / g / h / i / j
             / k / l / m / n / o / p / q / r / s / t / u / v / w / x
             / (a - b - c - d - e - f - g - h - i - j - k - l - m - n - o - p
                  - q - r - s - t - u - v - w - x - y - z - a - b - c - d - e
                  - f - g - h - i - j - k - l - m - n - o - p - q - r - s - t
                  - u - v - w - x - y - z - a - b - c - d - e - f - g - h - i
                  - j - k - l - m - n - o - p - q - r - s - t - u - v - w - x
                  - y - z - a - b - c - d - e - f - g - h - i - j - k - l - m
                  - n - o - p - q - r - s - t - u - v - w - x - y - z)
             / y / z;
}" (DEFUN JSTEST (A B) (/ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D (+ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X (DEFUN JSTESTINNER NIL (* A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)) Y Z) E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X (- A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) Y Z))))