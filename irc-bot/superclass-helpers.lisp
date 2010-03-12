(in-package :nisp.i)

(defun add-superclass (instance new-superclass)
  "Add NEW-SUPERCLASS to INSTANCE's superclass list."
  (reinitialize-instance (class-of instance) :direct-superclasses
                         (adjoin (find-class new-superclass)
                                  (class-direct-superclasses
                                   (class-of instance)))))

(defun delete-superclass-of (instance superclass)
  "Delete SUPERCLASS from INSTANCE's superclass list."
  (reinitialize-instance (class-of instance) :direct-superclasses
                         (remove (find-class superclass)
                                 (class-direct-superclasses
                                  (class-of instance)))))

;;; END
