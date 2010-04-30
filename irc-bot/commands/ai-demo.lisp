;;; Temp location for ai related code until command definitions get a bit
;;; less internal to nisp and more external.
;;;
;;; This stuff will not be loaded into the running nisp for the time
;;; being.
(in-package :nisp.i)


(defun random-up-or-down (&optional (magnitude 1))
  (if (zerop (random 2))
      (- magnitude)
      magnitude))

(defun generate-markov-sequence (&key (initial 0) (range 10))
  "Demo markov to get the idea of it."
  (iter (for up-or-down = (random-up-or-down))
        (for n initially initial then (+ n up-or-down))
        (until (or (> n (+ initial range)) (< n (+ initial (- range)))))
        (collect (list n up-or-down))))


(define-simple-command markov
  (reply (remove #\Newline
                 (format nil "~A" (generate-markov-sequence
                                   :initial 0 :range 3)))))


(defvar *states* (list :positive-even :positive-odd :negative-odd :negative-even :zero))
;;=> *STATES*

(defvar *learning-rate* 1/10
  "Rate that we adjust things based on a reward.")

(defvar *actions* (list (constantly "Probably")
                        (constantly "Bye")
                        (constantly "Hi")
                        (constantly "Interesting")
                        (constantly "ZzzZzzZzzZzz")))


(flet ((generate-action-list (&optional (initial-score 5) (action-set *actions*))
         (mapcar (lambda (x) (cons x initial-score)) action-set)))
  (defparameter *lookup-table* (alist-hash-table
                                `((:positive-even . ,(generate-action-list))
                                  (:positive-odd . ,(generate-action-list))
                                  (:zero . ,(generate-action-list))
                                  (:negative-even . ,(generate-action-list))
                                  (:negative-odd . ,(generate-action-list))))
    "We default the score for every entry to 5. This is chosen by the
    programmer. We have to start with some default. Our default is flat
    between the range 0 and 10. These are arbitrary values"))

(defun state-action-score (state action)
  "Scoring for a particular state/action pair."
  (cdr (assoc action (gethash state *lookup-table*))))


(defun (setf state-action-score) (score state action)
  (setf (cdr (assoc action (gethash state *lookup-table*)))
        score))


(defun state-learning-rate (state action)
  *learning-rate*)

(defun state-action-max-score (state action)
  10)

(defun compute-new-score (state action reward)
  (+ (state-action-score state action)
     (* (state-learning-rate state action)
        (+ reward
           (- (state-action-score state action))))))

(defun compute-state (number-input)
  (let ((number-input (if (stringp number-input)
                          (parse-integer number-input)
                          number-input)))
    (cond
      ((zerop number-input) :zero)
      ((and (> 0 number-input) (evenp number-input)) :negative-even)
      ((and (> 0 number-input) (oddp number-input)) :negative-odd)
      ((evenp number-input) :positive-even)
      (t :positive-odd))))


(defun flux (normal-score)
  (+ normal-score (/ (random 150) 100)))

(defun compute-possible-actions (string-integer)
  (aif (parse-integer string-integer :junk-allowed t)
       (mapcar (lambda (action)
                 (cons (state-action-score
                         (compute-state it) action) action)) *actions*)
       "I expect integers only! Your brainzzzzzzzz plox!"))

(defun add-flux (action-list)
  (mapcar (lambda (action-score-pair)
            (cons (flux (car action-score-pair))
                  (cdr action-score-pair)))
          action-list))

(defun compute-best-action (action-list)
  (sort action-list #'> :key #'car))

(defun handle-ai (string-integer)
  (aif (string-integer-p string-integer)
       (car (compute-best-action (add-flux (compute-possible-actions string-integer))))
       "I expect integers only!"))

(let ((input "")
      (last-score 0)
      (last-action nil)
      (last-state nil))
  (define-simple-command ai
    (if (string-integer-p (remaining-parameters))
        (progn
          (setq input (remaining-parameters))
          (destructuring-bind (score . action) (handle-ai (remaining-parameters))
            (setq last-action action
                  last-score score
                  last-state (compute-state (parse-integer (remaining-parameters))))
            (reply (remove #\Newline
                           (funcall action)))))
        (network-tree::next-node)))
  (define-simple-command ai-test-state
    (reply (princ-to-string (compute-state (parse-integer (remaining-parameters))))))
  (define-simple-command ai-previnfo
    (reply (list input last-score last-action)))
  (define-simple-command ai-test
    (network-tree::next-node))
  (define-simple-command ai-test-reward
    (if (string-integer-p (remaining-parameters))
        (reply "New score for state ~A and action ~A would be ~A."
               last-state
               (funcall last-action)
               (float (compute-new-score last-state last-action (parse-integer (remaining-parameters)))))
        (reply "Come on! Integers rock!")))
  (flet ((format-reward-reply (score)
           (format nil "Score for state ~A saying ~A is now ~A."
                   last-state
                   (funcall last-action)
                   (float (reward-ai last-state last-action score)))))
    (define-simple-command ai-trout
      (reply (format-reward-reply 0)))
    (define-simple-command ai-cookie
      (reply (format-reward-reply 10)))))

(defun reward-ai (state action amount)
  (setf (state-action-score state action)
        (compute-new-score state action amount)))
(define-simple-command test
  (network-tree::next-node))

(define-simple-command test-action
  (reply (funcall (nth (random 5) *actions*))))

(define-simple-command test-tokenize
  (network-tree::next-node))

#+ () (define-simple-command test-tokenize-word
  (with-input-from-string (s (remaining-parameters))
    (reply (nisp.tokenize::parse-word s))))

(define-simple-command tokenize
  (reply "~S" (nisp.tokenize:tokenize-string (remaining-parameters))))

(define-simple-command test-wordinfo
  (aif (gethash (remaining-parameters) wiktionary::*dictionary*)
       (reply "~A types: ~A"
              (wiktionary::word-name it)
              (wiktionary::word-pos it))
       (reply "I don't know!")))

(define-simple-command word
  (network-tree::next-node))

(define-simple-command word-pos
  (multiple-value-bind (pos-list found?)
      (wiktionary:lookup-pos (remaining-parameters))
    (if found?
        (reply "~A" pos-list)
        (reply "I can't find that word!"))))
