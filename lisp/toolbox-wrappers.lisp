(in-package :hermes.toolbox)

(defclass process-info ()
  ((serial-number :initarg :serial-number :accessor process-serial-number)
   (type :initarg :type :accessor process-type)
   (name :initarg :name :accessor process-name)))

(defun %psn->list (psn-ptr)
  (list (ccl:rref psn-ptr :processserialnumber.highlongofpsn)
        (ccl:rref psn-ptr :processserialnumber.lowlongofpsn)))

(defun %process-name (psn-ptr)
  (format nil "PSN-~8,'0X:~8,'0X"
          (ccl:rref psn-ptr :processserialnumber.highlongofpsn)
          (ccl:rref psn-ptr :processserialnumber.lowlongofpsn)))

(defun list-processes ()
  (ccl:rlet ((psn :processserialnumber))
    (let ((results '()))
      (when (zerop (#_GetCurrentProcess psn))
        (loop
          (push (make-instance 'process-info
                                :serial-number (%psn->list psn)
                                :type :application
                                :name (%process-name psn))
                results)
          (when (not (zerop (#_GetNextProcess psn)))
            (return)))
        (nreverse results)))))

(defun find-process-by-name (name)
  (find name (list-processes) :key #'process-name :test #'string-equal))

(defmacro with-apple-event-handler ((event-class event-id handler &key (system-target nil)) &body body)
  `(let ((descriptor (ccl:make-routine-descriptor ,handler :appleevent-handler)))
     (#_AEInstallEventHandler ,event-class ,event-id descriptor 0 ,system-target)
     (unwind-protect (progn ,@body)
       (#_AERemoveEventHandler ,event-class ,event-id descriptor ,system-target)
       (ccl:dispose-routine-descriptor descriptor))))

(defun make-apple-event (target event-class event-id &key (return-id 0) (transaction-id 0))
  (let ((event (ccl:make-record :appleevent)))
    (ccl:errchk (#_AECreateAppleEvent event-class event-id target return-id transaction-id event))
    event))

(defun dispatch-apple-event (event)
  (ccl:errchk (#_AEDispatchAppleEvent event (#_NewAEEventHandlerUPP #'ccl:event-dispatch) 0)))

(defun install-apple-event-handlers (handlers)
  (dolist (entry handlers)
    (destructuring-bind (event-class event-id function) entry
      (#_AEInstallEventHandler event-class event-id (ccl:make-routine-descriptor function :appleevent-handler) 0 nil))))

(defparameter *scheduled-tasks* (make-hash-table :test #'equal))

(defun register-scheduled-task (name function interval)
  (let ((process (start-background-task name function :sleep-time interval)))
    (setf (gethash name *scheduled-tasks*) (list :process process :interval interval))
    process))

(defun cancel-scheduled-task (name)
  (when (gethash name *scheduled-tasks*)
    (stop-background-task name)
    (remhash name *scheduled-tasks*)))

(defclass tcp-session ()
  ((host :initarg :host :reader tcp-session-host)
   (port :initarg :port :reader tcp-session-port)
   (driver-ref :initarg :driver-ref :accessor tcp-session-driver)
   (stream :initarg :stream :accessor tcp-session-stream)))

(defun ensure-mactcp-started ()
  (with-hermes-log ()
    (format t "Ensuring MacTCP stack is initialized for Telnet and modem sessions.~%"))
  (unless (gethash :mactcp *scheduled-tasks*)
    (ccl:errchk (#_OpenDriver :|.IPP|))
    (setf (gethash :mactcp *scheduled-tasks*) t)))

(defun open-tcp-stream (host port)
  (ensure-mactcp-started)
  (with-hermes-log ()
    (format t "Opening TCP session to ~A:~A.~%" host port))
  (make-instance 'tcp-session :host host :port port :driver-ref :mactcp :stream nil))

(defun close-tcp-stream (session)
  (when session
    (with-hermes-log ()
      (format t "Closing TCP session for ~A:~A.~%"
              (tcp-session-host session) (tcp-session-port session))))
  (setf (tcp-session-stream session) nil))

(defmacro with-tcp-stream ((var host port) &body body)
  `(let ((,var (open-tcp-stream ,host ,port)))
     (unwind-protect (progn ,@body)
       (close-tcp-stream ,var))))
