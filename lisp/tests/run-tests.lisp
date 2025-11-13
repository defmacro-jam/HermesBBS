;;; Simple SBCL test harness for Hermes Lisp scaffolding.

;; Ensure we load files relative to repository root when invoked via --script.
(when *load-truename*
  (let* ((script-path *load-truename*)
         (script-dir (make-pathname :name nil :type nil :defaults script-path))
         (repo-root (truename (merge-pathnames #P"../" script-dir))))
    (setf *default-pathname-defaults* repo-root)))

(load "lisp/hermes-system.lisp")

(load-hermes-bbs)

(defun assert-true (value message)
  (unless value
    (error "Assertion failed: ~A" message)))

(defun run-utilities-tests ()
  (format t "Running utilities tests...~%")
  (let ((state (hermes.common:ensure-hermes-state)))
    (assert-true (typep state 'hermes.common:hermes-state)
                 "ensure-hermes-state should create a Hermes state"))
  (hermes.utilities:initialize-logging)
  (let ((seeded (hermes.utilities:seed-global-state)))
    (assert-true (> (hermes.common:hermes-state-global-seed seeded) 0)
                 "Global seed should be positive"))
  t)

(defun run-networking-tests ()
  (format t "Running networking tests...~%")
  (hermes.networking:initialize-networking)
  (assert-true (hermes.common:hermes-state-network-ready-p
               (hermes.common:ensure-hermes-state))
               "Network ready flag should be set after initialization")
  (hermes.networking:start-telnet-service :host "example.com" :port 2323)
  (assert-true (typep hermes.networking::*telnet-session* 'hermes.toolbox:tcp-session)
               "Telnet session should be a TCP session instance")
  (hermes.networking:start-modem-service)
  (assert-true (gethash :modem-poll hermes.toolbox::*scheduled-tasks*)
               "Modem poll task should be registered")
  (hermes.networking:shutdown-networking)
  (assert-true (not (hermes.common:hermes-state-network-ready-p
                     (hermes.common:ensure-hermes-state)))
               "Network ready flag should be cleared after shutdown")
  t)

(defun run-ui-tests ()
  (format t "Running UI tests...~%")
  (hermes.ui:initialize-ui)
  (assert-true (gethash :ui-refresh hermes.toolbox::*scheduled-tasks*)
               "UI refresh task should be registered")
  (hermes.toolbox:cancel-scheduled-task :ui-refresh)
  t)

(defun run-background-task-tests ()
  (format t "Running background task tests...~%")
  (let* ((counter 0)
         (process (hermes.common:start-background-task
                   "test-task"
                   (lambda () (incf counter))
                   :sleep-time 0.01)))
    (sleep 0.05)
    (hermes.common:stop-background-task "test-task")
    (assert-true (>= counter 1) "Background task should execute at least once")
    (hermes.common:stop-background-task "test-task")
    (declare (ignore process)))
  t)

(defun run-resource-tests ()
  (format t "Running resource tests...~%")
  (let ((resources (hermes.main::open-required-resources)))
    (assert-true (listp resources) "Resource list should be returned"))
  t)

(defun run-tests ()
  (format t "Starting Hermes SBCL compatibility tests...~%")
  (run-utilities-tests)
  (run-networking-tests)
  (run-ui-tests)
  (run-background-task-tests)
  (run-resource-tests)
  (hermes.utilities:run-shutdown-hooks)
  (format t "All tests completed successfully.~%"))

(run-tests)
