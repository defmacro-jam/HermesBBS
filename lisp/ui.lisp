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

;;; ---------------------------------------------------------------------------
;;; Dialog flow modelling for the legacy Pascal UI

(defclass dialog-flow ()
  ((name :initarg :name :reader dialog-flow-name)
   (resource-id :initarg :resource-id :reader dialog-flow-resource-id)
   (controls :initarg :controls :accessor dialog-flow-controls :initform '())
   (transitions :initarg :transitions :accessor dialog-flow-transitions :initform '())
   (context :initarg :context :accessor dialog-flow-context
            :initform (make-hash-table :test #'equal)))
  (:documentation
   "Represents an MCL dialog derived from a Pascal dialog resource."))

(defmethod initialize-instance :after ((dialog dialog-flow) &key)
  (setf (dialog-flow-controls dialog) (copy-list (dialog-flow-controls dialog))))

(defclass user-editor-dialog (dialog-flow) ()
  (:documentation
   "Flow modelling for the Global User Editor (dialog resource 150)."))

(defclass user-list-dialog (dialog-flow)
  ((rows :initarg :rows :accessor user-list-dialog-rows :initform '()))
  (:documentation "Wraps the GUList LNew table from UserManager.p."))

(defclass user-search-dialog (dialog-flow) ()
  (:documentation "Represents the Global User Search criteria sheet."))

(defclass dialog-control ()
  ((name :initarg :name :reader dialog-control-name)
   (type :initarg :type :reader dialog-control-type)
   (pascal-control :initarg :pascal-control :reader dialog-control-pascal-control)
   (lisp-slot :initarg :lisp-slot :reader dialog-control-lisp-slot :initform nil)
   (help-text :initarg :help-text :reader dialog-control-help-text :initform "")
   (parser :initarg :parser :accessor control-parser :initform #'identity)
   (formatter :initarg :formatter :accessor control-formatter :initform #'identity)
   (default-value :initarg :default-value :accessor control-default-value :initform nil))
  (:documentation "Base control mapping between Pascal dialog items and Lisp."))

(defclass security-toggle-control (dialog-control) ()
  (:documentation "Specialised control for SL/DSL coupling."))

(defclass birthdate-control (dialog-control) ()
  (:documentation "Aggregates BirthDay/BirthMonth/BirthYear fields."))

(defclass statistic-control (dialog-control)
  ((daily-slot :initarg :daily-slot :reader statistic-control-daily-slot)
   (aggregate :initarg :aggregate :reader statistic-control-aggregate :initform :sum))
  (:documentation "Encodes legacy statistic counters with daily mirrors."))

(defclass search-control (dialog-control)
  ((criterion-key :initarg :criterion-key :reader search-control-key)
   (operator :initarg :operator :accessor search-control-operator :initform :equals)
   (value :accessor search-control-value :initform nil)
   (allowed-operators :initarg :allowed-operators :reader search-control-allowed-operators
                      :initform '(:equals :contains :greater-than :less-than)))
  (:documentation "Search criterion derived from GlobalUSearch record fields."))

(defun dialog-context-value (dialog key &optional default)
  (multiple-value-bind (value present-p)
      (gethash key (dialog-flow-context dialog))
    (if present-p value default)))

(defun (setf dialog-context-value) (value dialog key)
  (setf (gethash key (dialog-flow-context dialog)) value))

(defun find-control (dialog name)
  (or (find name (dialog-flow-controls dialog)
            :key #'dialog-control-name :test #'eq)
      (error "Unknown control ~S for dialog ~A." name (dialog-flow-name dialog))))

(defgeneric handle-dialog-event (dialog control event &key &allow-other-keys))

(defgeneric compile-search-criteria (dialog &key))

(defmethod handle-dialog-event ((dialog dialog-flow) (control dialog-control) (event (eql :update))
                                 &key value record &allow-other-keys)
  (let ((parsed (funcall (control-parser control) value)))
    (when (and record (dialog-control-lisp-slot control))
      (setf (slot-value record (dialog-control-lisp-slot control)) parsed))
    parsed))

(defun %split-string (string delimiter)
  (let ((result '())
        (start 0)
        (length (length string)))
    (loop for position = (position delimiter string :start start)
          do (if position
                 (progn
                   (push (subseq string start position) result)
                   (setf start (1+ position)))
                 (return (nreverse (cons (subseq string start length) result)))))))

(defun parse-optional-integer (value)
  (cond
    ((null value) nil)
    ((integerp value) value)
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab) value)))
       (if (zerop (length trimmed))
           nil
           (handler-case (parse-integer trimmed :junk-allowed nil)
             (error ()
               (error "Unable to parse integer from ~S." value))))))
    (t (error "Unsupported integer representation ~S." value))))

(defun parse-optional-string (value)
  (when value
    (let* ((string (etypecase value
                     (string value)
                     (symbol (string value))
                     (character (string value))))
           (trimmed (string-trim '(#\Space #\Tab) string)))
      (unless (zerop (length trimmed))
        trimmed))))

(defun ensure-security-level (value)
  (let ((level (parse-optional-integer value)))
    (unless level
      (error "Security level cannot be blank."))
    (when (or (< level 0) (> level 255))
      (error "Security level ~A out of 0-255 range." level))
    level))

(defun normalize-two-digit-year (year)
  (if (>= year 100)
      year
      (+ 1900 year)))

(defun parse-birthdate-string (value)
  (let ((input (parse-optional-string value)))
    (unless input
      (error "Birthdate field cannot be empty."))
    (labels ((validate (year month day)
               (unless (<= 1 month 12)
                 (error "Month ~A is outside of 1-12." month))
               (unless (<= 1 day 31)
                 (error "Day ~A is outside of 1-31." day))
               (values year month day)))
      (cond
        ((position #\- input)
         (let ((parts (%split-string input #\-)))
           (unless (= (length parts) 3)
             (error "Birthdate ~S must contain year-month-day." input))
           (destructuring-bind (year month day) parts
             (let ((y (normalize-two-digit-year (parse-integer year :junk-allowed nil)))
                   (m (parse-integer month :junk-allowed nil))
                   (d (parse-integer day :junk-allowed nil)))
               (validate y m d)))))
        ((position #\/ input)
         (let ((parts (%split-string input #\/)))
           (unless (= (length parts) 3)
             (error "Birthdate ~S must contain month/day/year." input))
           (destructuring-bind (month day year) parts
             (let ((y (normalize-two-digit-year (parse-integer year :junk-allowed nil)))
                   (m (parse-integer month :junk-allowed nil))
                   (d (parse-integer day :junk-allowed nil)))
               (validate y m d)))))
        (t (error "Unrecognised birthdate format ~S." input))))))

(defun compute-age (year month day)
  (multiple-value-bind (second minute hour current-day current-month current-year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (let ((age (- current-year year)))
      (when (or (< current-month month)
                (and (= current-month month)
                     (< current-day day)))
        (decf age))
      (max age 0))))

(defun string-contains-ci (haystack needle)
  (let ((upper-haystack (string-upcase haystack))
        (upper-needle (string-upcase needle)))
    (not (null (search upper-needle upper-haystack :test #'char=)))))

(defun ensure-numeric-delta (value)
  (let ((delta (parse-optional-integer value)))
    (unless delta
      (error "Statistic delta cannot be blank."))
    delta))

(defmethod handle-dialog-event ((dialog user-editor-dialog)
                                 (control security-toggle-control)
                                 (event (eql :toggle))
                                 &key value record &allow-other-keys)
  (let ((level (ensure-security-level value)))
    (when (and record (typep record 'user-record))
      (setf (slot-value record 'sl) level)
      (when (< (slot-value record 'dsl) level)
        (setf (slot-value record 'dsl) level)))
    (setf (dialog-context-value dialog :last-security-level)
          level)
    level))

(defmethod handle-dialog-event ((dialog user-editor-dialog)
                                 (control birthdate-control)
                                 (event (eql :update))
                                 &key value record &allow-other-keys)
  (multiple-value-bind (year month day)
      (parse-birthdate-string value)
    (let ((age (compute-age year month day)))
      (when (and record (typep record 'user-record))
        (setf (slot-value record 'birth-year) (code-char (mod year 256))
              (slot-value record 'birth-month) (code-char month)
              (slot-value record 'birth-day) (code-char day)
              (slot-value record 'age) age))
      (let ((summary (list :year year :month month :day day :age age :source value)))
        (setf (dialog-context-value dialog :last-birthdate) summary)
        summary))))

(defmethod handle-dialog-event ((dialog user-editor-dialog)
                                 (control statistic-control)
                                 (event (eql :increment))
                                 &key value record &allow-other-keys)
  (let ((delta (ensure-numeric-delta value)))
    (when (and record (dialog-control-lisp-slot control))
      (let* ((slot (dialog-control-lisp-slot control))
             (daily-slot (statistic-control-daily-slot control)))
        (incf (slot-value record slot) delta)
        (when daily-slot
          (incf (slot-value record daily-slot) delta))
        (let ((snapshot (list :delta delta
                               :total (slot-value record slot)
                               :daily (when daily-slot (slot-value record daily-slot)))))
          (setf (dialog-context-value dialog (dialog-control-name control)) snapshot)
          snapshot))))))

(defmethod handle-dialog-event ((dialog user-search-dialog)
                                 (control search-control)
                                 (event (eql :update))
                                 &key value &allow-other-keys)
  (let ((parsed (funcall (control-parser control) value)))
    (setf (search-control-value control) parsed)
    (setf (dialog-context-value dialog (search-control-key control))
          (list :operator (search-control-operator control)
                :value parsed))
    parsed))

(defmethod handle-dialog-event ((dialog user-search-dialog)
                                 (control search-control)
                                 (event (eql :operator))
                                 &key value &allow-other-keys)
  (unless (member value (search-control-allowed-operators control))
    (error "Operator ~S not valid for criterion ~S." value (search-control-key control)))
  (setf (search-control-operator control) value)
  (let ((current (dialog-context-value dialog (search-control-key control) nil)))
    (when current
      (setf (getf current :operator) value)
      (setf (dialog-context-value dialog (search-control-key control)) current)))
  value)

(defun evaluate-criterion (field-value control)
  (let ((criterion-value (search-control-value control))
        (operator (search-control-operator control)))
    (cond
      ((and (stringp criterion-value) (stringp field-value))
       (case operator
         (:equals (string-equal field-value criterion-value))
         (:contains (string-contains-ci field-value criterion-value))
         (t (error "Unsupported string operator ~S." operator))))
      ((and (numberp criterion-value) (numberp field-value))
       (case operator
         (:equals (= field-value criterion-value))
         (:greater-than (> field-value criterion-value))
         (:less-than (< field-value criterion-value))
         (t (error "Unsupported numeric operator ~S." operator))))
      ((typep criterion-value 'boolean)
       (case operator
         (:equals (eql field-value criterion-value))
         (t (error "Unsupported boolean operator ~S." operator))))
      ((null criterion-value) t)
      (t (error "Cannot evaluate criterion ~S against value ~S."
                (search-control-key control) field-value)))))

(defmethod compile-search-criteria ((dialog user-search-dialog) &key)
  (let* ((controls (remove-if-not (lambda (control) (typep control 'search-control))
                                  (dialog-flow-controls dialog)))
         (active (remove-if (lambda (control)
                              (null (search-control-value control)))
                            controls)))
    (if (null active)
        (lambda (record)
          (declare (ignore record))
          t)
        (lambda (record)
          (every (lambda (control)
                   (let ((slot (dialog-control-lisp-slot control)))
                     (unless slot
                       (error "Search control ~S is not bound to a record slot."
                              (dialog-control-name control)))
                     (evaluate-criterion (slot-value record slot) control)))
                 active)))))

(defun filter-user-list (list-dialog search-dialog)
  (let* ((predicate (compile-search-criteria search-dialog))
         (rows (user-list-dialog-rows list-dialog))
         (results (remove-if-not predicate rows)))
    (setf (dialog-context-value list-dialog :filtered-rows) results)
    (with-hermes-log ()
      (format t "Filtered ~D user rows down to ~D via Global User Search.~%"
              (length rows) (length results)))
    results))

(defun make-user-editor-dialog ()
  (make-instance 'user-editor-dialog
                 :name "Global User Editor"
                 :resource-id 150
                 :controls (list
                            (make-instance 'security-toggle-control
                                           :name :security-level-toggle
                                           :type :checkbox
                                           :pascal-control "SecurityLevel.OnOff/Value"
                                           :lisp-slot 'sl
                                           :parser #'parse-optional-integer
                                           :help-text
                                           "Checkbox + numeric pair mapped from SecurityLevel controls (items 12/13) in UserManager.p.")
                            (make-instance 'dialog-control
                                           :name :download-security-level
                                           :type :numeric-field
                                           :pascal-control "DownloadSL.Value"
                                           :lisp-slot 'dsl
                                           :parser #'parse-optional-integer
                                           :help-text
                                           "Numeric field tied to DownloadSL.Value in the Pascal dialog; stored in user-record DSL.")
                            (make-instance 'birthdate-control
                                           :name :birthdate-field
                                           :type :text-field
                                           :pascal-control "BirthDay/BirthMonth/BirthYear composite"
                                           :help-text
                                           "Aggregates the BirthDay, BirthMonth, and BirthYear char fields into a single edit text.")
                            (make-instance 'statistic-control
                                           :name :messages-posted-statistic
                                           :type :numeric-field
                                           :pascal-control "MessagesPosted.Value"
                                           :lisp-slot 'messages-posted
                                           :daily-slot 'messages-posted-today
                                           :parser #'parse-optional-integer
                                           :help-text
                                           "Spinner mapped from MessagesPosted.Value to adjust total and today counters.")
                            (make-instance 'statistic-control
                                           :name :download-k-statistic
                                           :type :numeric-field
                                           :pascal-control "DownloadK.Value"
                                           :lisp-slot 'downloaded-k
                                           :daily-slot 'downloaded-k-today
                                           :parser #'parse-optional-integer
                                           :aggregate :bandwidth
                                           :help-text
                                           "Numeric entry tied to DownloadK.Value updating kilobyte aggregates."))
                 :transitions '((:close . :user-list)
                                (:show-search . :user-search))))

(defmethod initialize-instance :after ((dialog user-list-dialog) &key)
  (setf (dialog-context-value dialog :rows) (user-list-dialog-rows dialog)))

(defun make-user-list-dialog (&key (rows '()))
  (make-instance 'user-list-dialog
                 :name "Global User List"
                 :resource-id 150
                 :rows rows
                 :controls (list
                            (make-instance 'dialog-control
                                           :name :user-list
                                           :type :list-view
                                           :pascal-control "Dialog item 2 (GUList LNew table)"
                                           :help-text
                                           "Primary list view created via LNew in Open_GlobalUEdit.")
                            (make-instance 'dialog-control
                                           :name :search-summary
                                           :type :static-text
                                           :pascal-control "Dialog item 22 (Status label)"
                                           :help-text
                                           "Static text updated by Update_GlobalUEdit to mirror search status."))
                 :transitions '((:double-click . :user-editor)
                                (:apply-search . :user-search))))

(defmethod initialize-instance :after ((dialog user-search-dialog) &key)
  (setf (dialog-context-value dialog :criteria) '()))

(defun make-user-search-dialog ()
  (make-instance 'user-search-dialog
                 :name "Global User Search"
                 :resource-id 150
                 :controls (list
                            (make-instance 'search-control
                                           :name :security-level-criterion
                                           :criterion-key :security-level
                                           :type :numeric-field
                                           :pascal-control "SecurityLevel.Value"
                                           :lisp-slot 'sl
                                           :parser #'parse-optional-integer
                                           :allowed-operators '(:equals :greater-than :less-than)
                                           :help-text
                                           "SecurityLevel Value edit control powering range comparisons.")
                            (make-instance 'search-control
                                           :name :city-filter
                                           :criterion-key :city
                                           :type :text-field
                                           :pascal-control "City.Value"
                                           :lisp-slot 'city
                                           :parser #'parse-optional-string
                                           :allowed-operators '(:equals :contains)
                                           :help-text
                                           "City.Value edit text used by the search pop-up list in the Pascal UI.")
                            (make-instance 'search-control
                                           :name :alias-filter
                                           :criterion-key :alias
                                           :type :text-field
                                           :pascal-control "Alias.Value"
                                           :lisp-slot 'alias
                                           :parser #'parse-optional-string
                                           :allowed-operators '(:equals :contains)
                                           :help-text
                                           "Alias.Value edit text enabling alias-based lookups."))
                 :transitions '((:apply . :user-list)
                                (:reset . :user-search))))

(defun make-sample-user (user-num name city security-level alias)
  (let ((record (make-instance 'user-record)))
    (setf (slot-value record 'user-num) user-num
          (slot-value record 'user-name) name
          (slot-value record 'city) city
          (slot-value record 'sl) security-level
          (slot-value record 'dsl) security-level
          (slot-value record 'alias) alias)
    record))

(defun run-user-dialog-scenarios ()
  (with-hermes-log ()
    (format t "Running scripted Global User Manager scenarios.~%"))
  (let* ((editor (make-user-editor-dialog))
         (user (make-instance 'user-record)))
    (setf (slot-value user 'user-name) "Wizard"
          (slot-value user 'alias) "WIZ"
          (slot-value user 'city) "Boston")
    (handle-dialog-event editor (find-control editor :security-level-toggle)
                         :toggle :value 75 :record user)
    (handle-dialog-event editor (find-control editor :download-security-level)
                         :update :value "80" :record user)
    (let ((birth-summary (handle-dialog-event editor (find-control editor :birthdate-field)
                                             :update :value "1978-03-14" :record user)))
      (assert (= (char-code (slot-value user 'birth-month)) 3))
      (assert (= (char-code (slot-value user 'birth-day)) 14))
      (assert (= (slot-value user 'age) (getf birth-summary :age))))
    (handle-dialog-event editor (find-control editor :messages-posted-statistic)
                         :increment :value 5 :record user)
    (handle-dialog-event editor (find-control editor :download-k-statistic)
                         :increment :value 120 :record user)
    (let* ((serialized (serialize-record user))
           (roundtrip (deserialize-record 'user-record serialized)))
      (assert (= 75 (slot-value roundtrip 'sl)))
      (assert (= 80 (slot-value roundtrip 'dsl)))
      (assert (= 5 (slot-value roundtrip 'messages-posted)))
      (assert (= 5 (slot-value roundtrip 'messages-posted-today)))
      (assert (= 120 (slot-value roundtrip 'downloaded-k)))
      (assert (= 120 (slot-value roundtrip 'downloaded-k-today)))
      (let* ((list-dialog (make-user-list-dialog
                           :rows (list roundtrip
                                       (make-sample-user 2 "Guest" "New York" 40 "GUEST"))))
             (search-dialog (make-user-search-dialog)))
        (handle-dialog-event search-dialog (find-control search-dialog :city-filter)
                             :operator :value :contains)
        (handle-dialog-event search-dialog (find-control search-dialog :city-filter)
                             :update :value "Bos")
        (handle-dialog-event search-dialog (find-control search-dialog :security-level-criterion)
                             :operator :value :greater-than)
        (handle-dialog-event search-dialog (find-control search-dialog :security-level-criterion)
                             :update :value "60")
        (let ((results (filter-user-list list-dialog search-dialog)))
          (assert (= 1 (length results)))
          (assert (eql (first results) roundtrip))
          (list :serialized serialized
                :roundtrip roundtrip
                :filtered results))))))
