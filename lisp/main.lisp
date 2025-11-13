(in-package :hermes.main)

(defun %open-resource (name)
  (let* ((path (resolve-relative-path name))
         (ref (ignore-errors (ccl:open-resource-file path :writable nil))))
    (when ref
      (update-hermes-state :resource-file path))
    ref))

(defun open-required-resources ()
  (remove nil
          (list (%open-resource "Hermes.r")
                (%open-resource "Hermes.proj.r"))))

(defun seed-random-state ()
  (let ((state (seed-global-state)))
    (setf (hermes-state-random-state state) (make-random-state t))
    (update-hermes-state :random-state (hermes-state-random-state state))
    state))

(defun start-background-maintenance ()
  (register-scheduled-task :mail-queue
                           (lambda ()
                             (with-hermes-log ()
                               (format t "Background mail queue flush.~%")))
                           10)
  (register-scheduled-task :statistics-snapshot
                           (lambda ()
                             (with-hermes-log ()
                               (format t "Capturing Hermes statistics snapshot.~%")))
                           60))

(defun request-hermes-shutdown ()
  (update-hermes-state :running nil)
  (run-shutdown-hooks))

(defun start-hermes (&key (telnet-host "localhost") (telnet-port 23))
  (initialize-logging)
  (bootstrap-initial-files)
  (load-node-preferences)
  (load-system-preferences)
  (open-required-resources)
  (seed-random-state)
  (initialize-networking)
  (initialize-ui)
  (start-telnet-service :host telnet-host :port telnet-port)
  (start-modem-service)
  (start-background-maintenance)
  (update-hermes-state :running t)
  (with-hermes-log ()
    (format t "Hermes entering main event loop.~%"))
  (loop :while (hermes-state-running-p (ensure-hermes-state))
        :do (ccl:event-dispatch *event-timeout*))
  (run-shutdown-hooks))
