(defpackage #:nisp.irc-types
  (:use :common-lisp :iterate :nisp.util-types))

(in-package :nisp.irc-types)

(defmacro ct (form type)
  `(let ((x ,form))
     (check-type x ,type 
                 (format nil "~S:~%~A" ',type 
                         (documentation ',type 'type)))))


(deftype ipv4-octet ()
  `(integer 0 255))


(defun length<= (sequence upto-count)
  "When SEQUENCE is longer then UPTO-COUNT return nil.

If SEQUENCE is shorter then UPTO-COUNT return its length."
  (declare (type sequence sequence)
           (type positive-fixnum upto-count))
  (loop for char across sequence
          counting char into count 
          when (> count upto-count) return nil
     finally (return count)))
#+ (or)
(progn
  (assert (length<= "aaaaa" 5))
  (assert (length<= "aaaa" 5)))

(deftype letter-char () 
  "Represents an uppercase or lowercase letter in ASCII."
  '(member #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
    #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D
    #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z))

(deftype dec-digit-char ()
  "Number character from 0 to 9 in base 10."
  '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(deftype hex-digit-char ()
  "Number character from 0 to F in base 16"
  '(or
    dec-digit-char
    (member #\A #\B #\C #\D #\E #\F)))

(deftype special-char ()
  "Represents extra non-digit/letter characters."
  '(member #\| #\[ #\] #\` #\^ #\\ #\_ #\{ #\}))

(deftype newline-char ()
  "Found at the end of a line in IRC messages.
In other words: \\\r\\\n"
  '(member #\Return #\Newline))

(deftype nickname-start-char ()
  "Valid character at the start of an IRC nickname."
  ;; These are taken from advice from duckinator on eighthbit.net/offtopic
  ;; Also see rfc 2812 sec: 2.3.1
  '(or 
    letter-char
    special-char))

(deftype nickname-char ()
  "Valid character after the first character of an IRC nickname."
  ;; Taken from advice from duckinator on eighthbit.net/offtopic
  '(or nickname-start-char
    (member dec-digit-char #\-)))
;; The meaning of these is specified in rfc2811
(deftype channel-start-char ()
  "Valid starting prefix of a channel name.

On most IRC networks # indicates a normal channel."
  ;; rfc2821 sec 1.3
  '(member #\& #\# #\+ #\!))

(deftype channel-char ()
  "Valid character in the name portion of a channel name."
  ;; rfc2821 sec 1.3
  `(and character
        (not (member #\Nul #\Bel #\, #\Space #\:))))

(deftype username-char ()
  "Valid char in the user portion.

In detail, this refers to the <user>@<hostmask> form."
  ;; Yes rfc2812 says #\Bel is ok here. Go figure.
  `(and character
        (not (or newline-char 
                 (member #\Nul #\Space #\@)))))

(deftype valid-integer-command ()
  "IRC numerical commands must be in the range 000 to 999."
  ;; This type does not verify that command is 001, only that it is 1
  `(integer 0 999))

;; Need some work to figure out how to validate strings.
(deftype valid-command ()
  "Commands must be an integer or a string.

Note that this type is incompletely defined."
  `(or valid-integer-command))

(deftype maximum-message-length ()
  "Longest SIZE an IRC message may be."
  '(integer 1 512))

(macrolet ((define-type-predicate (type-name &optional docstring)
             "Define simple predicates that check TYPE-NAME.
These predicates are the name of the type plus a trailing -p.

For example the type foo-bar will have a predicate named foo-bar-p that
takes one arguement.

These predicates also have the docstring of TYPE-NAME included in
addition to whatever DOCSTRING is given."
             (flet ((type-name-substr ()
                      (subseq (symbol-name type-name)
                              (- (length (symbol-name type-name)) 2))))
               (assert (not (string= (type-name-substr)
                                     "-P"))
                       (type-name) 
                       "~% Types should not end in ~S" (type-name-substr)))
             `(defun ,(intern (concatenate 'string (symbol-name type-name) "-P")) (object)
                ,(concatenate 'string 
                              (or docstring 
                                  "MISSING DOCSTRING: WRITE ME!")
                              "

Type documentation:
  " (or (documentation type-name 'type)
        "None provided."))
                (typep object ',type-name))))
  (define-type-predicate ipv4-octet)
  (define-type-predicate hex-digit-char)
  (define-type-predicate channel-start-char)
  (define-type-predicate maximum-message-length)
  (define-type-predicate dec-digit-char)
  (define-type-predicate newline-char)
  (define-type-predicate letter-char)
  (define-type-predicate nickname-start-char)
  (define-type-predicate special-char)
  (define-type-predicate channel-char)
  (define-type-predicate positive-fixnum)
  (define-type-predicate username-char)
  (define-type-predicate nickname-char))

(defun ipv4-octet-string-p (octet-string)
  (typep (read-from-string octet-string) 'ipv4-octet))

(deftype ipv4-octet-string ()
  `(satisfies ipv4-octet-string-p))

(defun ipv4-address-string-p (ipv4-string)
  (let ((sequence (split-sequence:split-sequence #\. ipv4-string)))
    (and (= (length sequence) 4)
         (every #'ipv4-octet-string-p sequence))))
(deftype ipv4-address-string ()
  '(satisfies ipv4-address-string-p))

(defun username-string-p (string)
  (every #'username-char-p string))
(deftype username-string ()
  "Represents the user part of <user>@<hostmask>."
  '(satisfies username-string-p))

(macrolet ((define-string-p 
               (name start-character-type character-type
                     &optional docstring)
             "Define predicates that check types of the form:
<START-CHARACTER-TYPE><CHARACTER-TYPE>+."
             `(defun ,name (string)
                ,(or docstring "MISSING DOCSTRING: WRITE ME!")
                (and (typep (char string 0) ',start-character-type)
                    (every
                     (lambda (x)
                       (declare (type character x))
                       (typep x ',character-type))
                     (subseq string 1))))))
  (define-string-p nickname-string-p nickname-start-char nickname-char)
  (define-string-p channel-string-p channel-start-char channel-char))

(deftype nickname-string ()
  `(and 
    (vector character)
    (satisfies nickname-string-p)))


;; Don't forget that these are also case insensitive.
(deftype channel-string (&optional size)
  "Valid channel name.

Needs to be m/[#!+&][^ \x007,:]+/"
  `(and 
    (vector character ,size)
    (satisfies channel-string-p)))

(iter (for (symbol state) :in-packages :nisp.irc-types :having-access (:internal))
      (when (or (type-specifier-p symbol)
                (fboundp symbol))
        (export symbol :nisp.irc-types)
        (collect symbol)))