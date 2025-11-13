(in-package :hermes.common)

(defparameter *hermes-root* (ccl:current-directory))

(defparameter *event-timeout* 0.1d0)

(defstruct hermes-state
  (startup-time (get-internal-real-time) :type integer)
  (resource-files '() :type list)
  (shutdown-hooks '() :type list)
  (global-seed (random most-positive-fixnum) :type integer)
  (random-state (make-random-state t) :type random-state)
  (network-ready-p nil :type boolean)
  (running-p t :type boolean))

(defparameter *hermes-state* nil)
(defparameter *background-processes* (make-hash-table :test #'equal))

(defun ensure-hermes-state ()
  (or *hermes-state*
      (setf *hermes-state* (make-hermes-state))))

(defun update-hermes-state (key value)
  (let ((state (ensure-hermes-state)))
    (ecase key
      (:resource-file (push value (hermes-state-resource-files state)))
      (:network-ready (setf (hermes-state-network-ready-p state) value))
      (:seed (setf (hermes-state-global-seed state) value))
      (:random-state (setf (hermes-state-random-state state) value))
      (:running (setf (hermes-state-running-p state) value)))
    state))

(defun timestamp-string (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defmacro with-hermes-log ((&optional (stream '*standard-output*)) &body body)
  `(let ((*print-pretty* t))
     (format ,stream "[~A] " (timestamp-string (get-universal-time)))
     ,@body))

(defun resolve-relative-path (relative)
  (merge-pathnames relative *hermes-root*))

(defmacro define-background-task (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (block ,name
       (loop
         (return-from ,name (progn ,@body))))))

(defun start-background-task (name function &key (sleep-time *event-timeout*) (priority 0) (stack-size 2048))
  (declare (ignore priority stack-size))
  (let ((process
          (ccl:process-run-function
           name
           (lambda ()
             (loop
               (funcall function)
               (ccl:process-sleep sleep-time))))))
    (setf (gethash name *background-processes*) process)
    process))

(defun stop-background-task (name)
  (let ((process (gethash name *background-processes*)))
    (when process
      (ccl:process-kill process)
      (remhash name *background-processes*)))
  t)

(in-package :hermes.utilities)

(defun initialize-logging ()
  (with-hermes-log ()
    (format t "Hermes logging initialized.~%")))

(defun seed-global-state ()
  (let ((state (ensure-hermes-state)))
    (setf (hermes-state-global-seed state) (get-internal-real-time))
    (update-hermes-state :seed (hermes-state-global-seed state))
    state))

(defun format-datestamp (universal-time)
  (timestamp-string universal-time))

(defun register-shutdown-hook (function)
  (let ((state (ensure-hermes-state)))
    (push function (hermes-state-shutdown-hooks state)))
  function)

(defun run-shutdown-hooks ()
  (dolist (hook (hermes-state-shutdown-hooks (ensure-hermes-state)))
    (ignore-errors (funcall hook)))
  (setf (hermes-state-shutdown-hooks (ensure-hermes-state)) nil))
