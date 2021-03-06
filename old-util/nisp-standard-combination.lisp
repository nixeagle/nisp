;;; Nisp standard method combination. This adds a few extra helpers above
;;; and beyond the normal method combination. This should be regarded as
;;; experimental and certainly not for use in anything critical.

(defpackage #:nisp-standard-method-combination
  (:use :cl :iterate :alexandria)
  (:export #:nisp-standard))

(in-package :nisp-standard-method-combination)

(defun lambda-list-keyword-p (x)
  #.(format nil "True if X is a lambda list keyword.

Any one of: ~S" lambda-list-keywords)
  (member x lambda-list-keywords))

(defun lambda-list-symbols (lambda-list)
  "Return just the argument names of LAMBDA-LIST.
t
All keywords and argument parameters are stripped returning a flat list
with only the argument names themselves."
  (declare (list lambda-list))
  (mapcar #'ensure-car
          (remove-if #'lambda-list-keyword-p
                     lambda-list)))

(defun method-specializers-unique-p (method list)
  "True if METHOD's specializers are unique among the methods on LIST."
  (flet ((method-signature (meth)
           (append (method-qualifiers meth)
                   (closer-mop:method-specializers meth))))
    (not (find (method-signature method) list :test #'equal
               :key #'method-signature))))

(defun check-unique-method-specializers (method list)
  (unless (method-specializers-unique-p method list)
    (error "Two methods are specialized on the same arguments.")))

(defun check-unique-method-specializers-list (methods)
  "Errors when two METHODS specialize on the same arguments."
  (declare (list methods))
  (when methods
    (check-unique-method-specializers (car methods) (cdr methods))
    (check-unique-method-specializers-list (cdr methods))))

(defun collect-normal-combinations
    (methods &rest combo-keywords)
  "Group METHODS by COMBO-KEYWORDS plus primary methods.

The return from this function is:
 (values primary-methods
        (first COMBO-KEYWORDS)
        (second COMBO-KEYWORDS)
        ...
        (last COMBO-KEYWORDS))

The values are all groups of methods that correspond to the qualifiers
passed in as COMBO-KEYWORDS.

For example if a method is an :AROUND method, and COMBO-KEYWORDS
contains :AROUND, that method (along with all others qualified as :AROUND
will be grouped together."
  (iter outer
        (with output-array = (make-array (length combo-keywords)
                                         :initial-element nil))
        (for method in methods)
        (for qualifiers = (method-qualifiers method))
        (for intersection = (intersection qualifiers combo-keywords))
        (cond
          ((length= 1 intersection)
           (iter (for keyword :in combo-keywords)
                 (for index :index-of-sequence combo-keywords)
                 (when (find keyword qualifiers)
                   (check-unique-method-specializers method (aref output-array (1+ index)))
                   (in outer (push method (aref output-array (1+ index))))))
           #+ ()             (collecting-into-forms qualifiers method combo-keywords gensyms))
          ((length= 0 intersection)
           (push method (aref output-array 0)))
          (t (error "Too many combination keywords for normal-combinations.")))
        (finally (return-from outer
                   (values-list (coerce output-array 'list))))))

(defun wrap-method (around form)
  "Generate a form where AROUND is wraps FORM.

This is used repeatedly in method combinations to achieve a form of
nesting where the inner groupings are reached by (call-next-method ...)."
  (if around
      `(call-method ,(first around)
                    (,@(rest around)
                       (make-method ,form)))
      form))

(defun generate-call-method-forms (methods)
  "Generate (call-method METHOD) from METHODS."
  (mapcar (lambda (method)
            `(call-method ,method))
          methods))

(define-method-combination nisp-standard (&key hook)
  ((methods *))
#+ ()  (:arguments &whole argument-list)
#+ ()  (:generic-function gf)
   "Alternate, more advanced standard method combination.

There are 6 different kinds of qualified methods. The 4 standard
qualifiers are `:around' `:before' `:after' and `primary'. These have the
expected behavior with the exception of `primary' when HOOK is non-nil.

`primary' when HOOK is non-nil is treated as a list of methods to run in
sequence just as GNU Emacs hooks behave. The value of HOOK should be the
name of a function to pass the results to. The behavior here is similar to
the simple-method-combinations specified in the ANSI specification with
one exception. Specifying additional qualifiers unrelated to any of the
qualifiers already used as part of this method combination strategy will
allow multiple methods specialized on the same arguments to be on the hook
at the same time.

For example if a generic function is specified as:

  (:method-combination nisp-standard :hook list)

All the applicable `primary' methods will run and each will have its
result placed in a list and returned as the result of the generic-function
call.

A useful way to get similar behavior to GNU Emacs'
`run-hook-with-args-until-success' is to define the method combination
with `:hook' as `or'. Then all methods on the hook will run until one
returns non-nil. To achieve behavior like
`run-hook-with-args-until-failure' define the method combination with
`:hook' set to `and' and all methods on the hook will then run until one
returns nil.

Do not forget that when HOOK is specified, primary methods may have the
same specializers so long as they have additional unrelated
qualifiers. The following two method defintions are valid in this case.

  (defmethod foo (arg1 arg2) 1)
  (defmethod foo :different-qualifier (arg1 arg2) 2)

Finally two additional qualifiers are supported:

 + `:meta-around' uses the same principle that asdf uses for their special
    around qualifier. This allows library code to use around methods
    without hindering the user's ability to add their own around
    methods. These methods get called before any `:around', `:before',
    `:after', or `primary' methods and are called in most-specific-first
    order, just as `:around' methods are.

 + `:defaulting' methods are mostly useful for specifying defaults for an
    entire generic function. These are called before any other methods,
    and are run in least-specific-first order, the _opposite_ of
    `:meta-around' and `:around' methods. The idea for this is taken from
    WRAPPING-STANDARD."
  (multiple-value-bind (primary defaulting meta-around
                                around before after)
      (collect-normal-combinations methods
       :defaulting :meta-around :around :before :after)
    (unless hook
      (check-unique-method-specializers-list primary))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(generate-call-method-forms before)
                                ,(if hook
                                     `(,hook ,@(mapcar (lambda (method)
                                                         `(call-method ,method))
                                                       primary))
                                     `(call-method ,(first primary)
                                                   ,(rest primary))))
                       ,@(generate-call-method-forms (reverse after)))
                    `(call-method ,(first primary)))))
      (wrap-method (reverse defaulting)     ; :defaulting
                   (wrap-method meta-around ; :meta-around
                                (wrap-method around form)))))) ; :around