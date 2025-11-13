(in-package :hermes.tests)

(defstruct (workflow-event
             (:constructor make-workflow-event (type &optional payload)))
  (type (error "Workflow events require a type."))
  (payload nil))

(defstruct (workflow-script
             (:constructor make-workflow-script (name events)))
  (name "Unnamed Script")
  (events '()))

(defstruct (workflow-context
             (:constructor make-workflow-context
                 (&key (state (make-hash-table :test #'eq))
                       (transcript (make-array 0 :adjustable t :fill-pointer 0)))))
  (state (make-hash-table :test #'eq))
  (transcript (make-array 0 :adjustable t :fill-pointer 0)))

(defun workflow-transcript-entries (context)
  "Return the execution transcript for CONTEXT as a list of entries."
  (coerce (workflow-context-transcript context) 'list))

(defun %record-transcript (context entry)
  (vector-push-extend entry (workflow-context-transcript context)))

(defparameter *workflow-handlers* (make-hash-table :test #'eq)
  "Registry associating workflow event types with handler functions.")

(defun register-workflow-handler (type function)
  "Register FUNCTION as the handler for workflow events of TYPE."
  (setf (gethash type *workflow-handlers*) function))

(defun %ensure-handler (type)
  (or (gethash type *workflow-handlers*)
      (error "No workflow handler defined for event ~S." type)))

(defun %set-slot-value (instance slot-key value)
  (multiple-value-bind (symbol status)
      (find-symbol (string slot-key) :hermes.storage)
    (declare (ignore status))
    (unless symbol
      (error "Unknown slot ~A for instance of ~S." slot-key (class-of instance)))
    (setf (slot-value instance symbol) value)))

(defun %populate-instance (instance slot-plist)
  (loop for (slot value) on slot-plist by #'cddr do
        (%set-slot-value instance slot value))
  instance)

(defun %serialize-record* (record)
  (length (serialize-record record)))

(defun %mac-seconds-from-universal (universal)
  (- universal #.(encode-universal-time 0 0 0 1 1 1904 0)))

(defun %ensure-user-record (context payload)
  (let* ((data (getf payload :user-data))
         (user (make-instance 'user-record)))
    (%populate-instance
     user
     (append data
             (list :first-on (%mac-seconds-from-universal (get-universal-time))
                   :last-on (%mac-seconds-from-universal (get-universal-time)))))
    (setf (gethash :current-user (workflow-context-state context)) user)
    (let ((bytes (%serialize-record* user)))
      (%record-transcript context
                          (list :event :login
                                :user (user-record-user-name user)
                                :serialized-bytes bytes
                                :security-level (user-record-sl user))))
    user))

(defun %handle-login (context event)
  (let ((user (%ensure-user-record context (workflow-event-payload event))))
    (with-hermes-log ()
      (format t "Test harness logged in ~A (SL ~D).~%"
              (user-record-user-name user)
              (user-record-sl user)))
    user))

(defun %ensure-message-record (context payload)
  (let* ((state (workflow-context-state context))
         (user (or (gethash :current-user state)
                   (error "Message event requires an authenticated user.")))
         (data (copy-list (getf payload :message-data)))
         (body (getf payload :body ""))
         (message (make-instance 'message-record)))
    (unless (getf data :from-user-number)
      (setf data (append data (list :from-user-number (user-record-user-num user)))))
    (unless (getf data :from-user-name)
      (setf data (append data (list :from-user-name (user-record-user-name user)))))
    (%populate-instance message data)
    (let ((stored (or (getf payload :stored-as) 1)))
      (setf (message-record-stored-as message) stored))
    (unless (post-ratio-ok-p user)
      (error "User ~A failed post-ratio checks in test harness." (user-record-user-name user)))
    (let ((messages (gethash :messages state)))
      (setf (gethash :messages state)
            (cons (list :record message :body body)
                  messages)))
    (incf (user-record-messages-posted user))
    (incf (user-record-messages-posted-today user))
    (let ((bytes (%serialize-record* message)))
      (%record-transcript context
                          (list :event :message
                                :title (message-record-title message)
                                :bytes bytes
                                :body-length (length body))))
    (with-hermes-log ()
      (format t "Test harness posted message '~A' on behalf of ~A.~%"
              (message-record-title message)
              (user-record-user-name user)))
    message))

(defun %reply-for-telnet-command (command option capabilities)
  (ecase command
    (:do (if (member option capabilities)
             (list :send :will option)
             (list :send :wont option)))
    (:dont (list :send :wont option))
    (:will (if (member option capabilities)
               (list :send :do option)
               (list :send :dont option)))
    (:wont (list :send :dont option))))

(defun %handle-telnet (context event)
  (let* ((payload (workflow-event-payload event))
         (incoming (getf payload :incoming))
         (capabilities (or (getf payload :capabilities)
                           '(:terminal-type :suppress-go-ahead :echo)))
         (responses (mapcar (lambda (command)
                              (%reply-for-telnet-command (first command)
                                                         (second command)
                                                         capabilities))
                            incoming)))
    (let ((expected (getf payload :expected)))
      (when expected
        (unless (equal expected responses)
          (error "Telnet negotiation mismatch: expected ~S, saw ~S"
                 expected responses))))
    (%record-transcript context
                        (list :event :telnet
                              :incoming incoming
                              :responses responses))
    (with-hermes-log ()
      (format t "Test harness processed ~D Telnet negotiation steps.~%"
              (length incoming)))
    (let ((state (workflow-context-state context)))
      (setf (gethash :telnet-handshake state) responses))
    responses))

(register-workflow-handler :login #'%handle-login)
(register-workflow-handler :message #'%ensure-message-record)
(register-workflow-handler :telnet #'%handle-telnet)

(defun execute-workflow-script (script &key (context (make-workflow-context)) verbose)
  "Execute SCRIPT, returning the updated workflow CONTEXT.
When VERBOSE is true each event result is logged via WITH-HERMES-LOG."
  (with-hermes-log ()
    (format t "Executing workflow script ~A containing ~D events.~%"
            (workflow-script-name script)
            (length (workflow-script-events script))))
  (dolist (event (workflow-script-events script) context)
    (let ((result (funcall (%ensure-handler (workflow-event-type event))
                           context event)))
      (when verbose
        (with-hermes-log ()
          (format t "Event ~S produced ~S.~%"
                  (workflow-event-type event)
                  result)))))
  context)

(defun make-login-event (&key user-data)
  (make-workflow-event :login (list :user-data user-data)))

(defun make-message-post-event (&key message-data body stored-as)
  (make-workflow-event :message (list :message-data message-data
                                      :body (or body "")
                                      :stored-as stored-as)))

(defun make-telnet-negotiation-event (&key incoming capabilities expected)
  (make-workflow-event :telnet (list :incoming incoming
                                     :capabilities capabilities
                                     :expected expected)))

(defun make-core-workflow-script ()
  "Construct the default script that exercises login, posting and Telnet negotiation."
  (let ((login (make-login-event
                :user-data (list :user-num 42
                                  :sl 80
                                  :dsl 80
                                  :user-name "TestUser"
                                  :password "SECRETS"
                                  :city "Cupertino"
                                  :state "CA"
                                  :messages-posted 10
                                  :total-logons 5)))
        (message (make-message-post-event
                  :message-data (list :title "Welcome"
                                      :to-user-number 1
                                      :to-user-name "SysOp"
                                      :deletable t
                                      :file-attached nil
                                      :date-entered 0)
                  :body "This is a synthetic post from the test harness."))
        (telnet (make-telnet-negotiation-event
                 :incoming '((:do :terminal-type)
                             (:will :suppress-go-ahead)
                             (:do :echo)
                             (:will :new-environment))
                 :capabilities '(:terminal-type :suppress-go-ahead :echo)
                 :expected '((:send :will :terminal-type)
                             (:send :do :suppress-go-ahead)
                             (:send :will :echo)
                             (:send :dont :new-environment)))))
    (make-workflow-script "Core Hermes workflows" (list login message telnet))))

(defun run-core-workflow-script (&key verbose)
  "Execute the built-in core workflow script and return the resulting context."
  (execute-workflow-script (make-core-workflow-script) :verbose verbose))

;;; Provide a shorthand alias for tests expecting RUN-CORE-WORKFLOWS.
(defun run-core-workflows (&key verbose)
  (run-core-workflow-script :verbose verbose))
