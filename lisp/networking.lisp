(in-package :hermes.networking)

(defparameter *telnet-session* nil)
(defparameter *modem-session* nil)

(defun initialize-networking ()
  (with-hermes-log ()
    (format t "Initializing Hermes networking stack (Telnet/Modem only, no AppleTalk).~%"))
  (ensure-mactcp-started)
  (register-shutdown-hook #'shutdown-networking)
  (update-hermes-state :network-ready t))

(defun start-telnet-service (&key (host "localhost") (port 23))
  (setf *telnet-session* (open-tcp-stream host port))
  (when *telnet-session*
    (register-scheduled-task :telnet-heartbeat
                             (lambda ()
                               (with-hermes-log ()
                                 (format t "Telnet heartbeat for ~A:~A.~%"
                                         (tcp-session-host *telnet-session*)
                                         (tcp-session-port *telnet-session*))))
                             5)))

(defun start-modem-service (&key (driver :|.MDEV|))
  (setf *modem-session* driver)
  (with-hermes-log ()
    (format t "Starting modem service via driver ~A.~%" driver))
  (ignore-errors (ccl:errchk (#_OpenDriver driver)))
  (register-scheduled-task :modem-poll
                           (lambda ()
                             (with-hermes-log ()
                               (format t "Polling modem carrier detect.~%")))
                           3))

(defun shutdown-networking ()
  (when *telnet-session*
    (close-tcp-stream *telnet-session*)
    (setf *telnet-session* nil))
  (setf *modem-session* nil)
  (cancel-scheduled-task :telnet-heartbeat)
  (cancel-scheduled-task :modem-poll)
  (with-hermes-log ()
    (format t "Hermes networking services stopped.~%"))
  (update-hermes-state :network-ready nil))
