(in-package :hermes.ui)

(defun initialize-ui ()
  (with-hermes-log ()
    (format t "Initializing Hermes UI subsystems (Message editors, Chat, Terminal).~%"))
  (display-splash)
  (register-scheduled-task :ui-refresh #'refresh-session-windows 1))

(defun display-splash ()
  (with-hermes-log ()
    (format t "Displaying Hermes splash screen.~%")))

(defun refresh-session-windows ()
  (with-hermes-log ()
    (format t "Refreshing active Hermes session windows.~%")))
