(in-package :cl-user)

#-ccl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ccl)
    (defpackage :ccl
      (:use :cl)
      (:export #:current-directory
               #:process-run-function
               #:process-sleep
               #:process-kill
               #:event-dispatch
               #:open-resource-file
               #:errchk
               #:defsystem
               #:load-system))))

#-ccl
(in-package :ccl)

#-ccl
(defun current-directory ()
  (truename *default-pathname-defaults*))

#-ccl
(defun process-run-function (name function)
  (declare (type function function))
  #+sb-thread
  (let ((thread (sb-thread:make-thread function :name (string name))))
    thread)
  #-sb-thread
  (progn
    (funcall function)
    nil))

#-ccl
(defun process-sleep (seconds)
  (sleep seconds))

#-ccl
(defun process-kill (process)
  #+sb-thread
  (when (typep process 'sb-thread:thread)
    (sb-thread:terminate-thread process))
  #-sb-thread
  (declare (ignore process))
  t)

#-ccl
(defun event-dispatch (timeout)
  (sleep timeout))

#-ccl
(defun open-resource-file (path &key writable)
  (declare (ignore writable))
  (when (probe-file path)
    path))

#-ccl
(defun errchk (value)
  value)

#-ccl
(defvar *registered-systems* (make-hash-table :test #'equal))

#-ccl
(defun %component-path (name default-path)
  (merge-pathnames (make-pathname :name name :type "lisp") default-path))

#-ccl
(defun defsystem (name &rest clauses)
  (setf (gethash name *registered-systems*) clauses)
  name)

#-ccl
(defun load-system (name)
  (let* ((clauses (gethash name *registered-systems*))
         (default-path (or (second (assoc :default-pathname clauses))
                           *default-pathname-defaults*))
         (components (second (assoc :components clauses))))
    (dolist (component components)
      (destructuring-bind (:file file &key depends-on) component
        (declare (ignore depends-on))
        (let ((pathname (%component-path file default-path)))
          (load pathname)))))
  t)

#-ccl
(in-package :cl-user)
