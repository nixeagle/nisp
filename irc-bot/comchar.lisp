(in-package :nisp.i)

(deftype valid-comchar ()
  "Usable characters for irc comchars.

Most anything else in the ASCII set can't be used as they occur as part
of normal conversation and an IRC bot that interferes with that is not
a very friendly bot."
  '(member #\! #\% #\) #\+ #\, #\- #\. #\<
    #\@ #\\ #\] #\_ #\` #\{ #\| #\} #\~))

(defun valid-comchar-string-p (string)
  (and (typep string '(and (string 1)))
       (typep (character string) 'valid-comchar)))

(deftype valid-comchar-string ()
  '(satisfies valid-comchar-string-p))

(defclass comchar ()
  ((comchar :type valid-comchar
            :accessor comchar
            :documentation "Single character that the program responds to.

Saying :comchar \"!\" or similar will work thanks to the shared-initialize."))
  (:documentation "Represents an irc bot comchar.

This is a single character, usually a symbol that the bot responds
to. This class will signal an error if a comchar is not of the type
valid-comchar.")
  (:default-initargs :comchar "\,"))

(defmethod (setf comchar) ((char string) (object comchar))
  "Set CHAR, a string of length 1 as the comchar."
  (declare (type (string 1) char))
  (setf (slot-value object 'comchar)
        (character char)))

(defmethod shared-initialize :after ((char comchar) slot-names &key comchar)
  "Make sure that :COMCHAR is a char before storing."
  (when comchar
    (setf (comchar char) comchar)))
