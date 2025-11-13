(in-package :hermes.file-areas)

(defconstant +mac-time-offset+ 126230400
  "Number of seconds between the Mac epoch (1904) and Common Lisp universal time (1900).")

(defun universal-time->mac-seconds (universal-time)
  "Convert a Common Lisp universal time stamp to the signed Mac seconds representation."
  (when universal-time
    (let* ((seconds (- universal-time +mac-time-offset+))
           (wrapped (if (> seconds #x7FFFFFFF)
                        (- seconds #x100000000)
                        seconds)))
      wrapped)))

(defun mac-seconds->universal (mac-seconds)
  "Convert a Mac seconds value back to universal time."
  (when mac-seconds
    (+ mac-seconds +mac-time-offset+)))

(defun %directory-pathname (path)
  "Normalize PATH so that pathname operations treat it as a directory."
  (let ((pn (pathname path)))
    (if (or (pathname-name pn) (pathname-type pn))
        (make-pathname :defaults pn
                       :name nil
                       :type nil)
        pn)))

(defun %default-directory-pattern (path)
  "Return a wildcard pathname that enumerates every regular file under PATH."
  (make-pathname :defaults path
                 :name :wild
                 :type :wild))

(defun %truncate-string (string max-length)
  (let ((as-string (string string)))
    (if (> (length as-string) max-length)
        (subseq as-string 0 max-length)
        as-string)))

(defun %file-byte-length (pathname)
  (ignore-errors
    (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun %directory-pathname-p (pathname)
  "Return true when PATHNAME denotes a directory."
  (or (null (pathname-name pathname))
      (eql (pathname-type pathname) :unspecific)))

(defun file-ok-mask (file-name mask)
  "Return true when FILE-NAME satisfies the wildcard MASK (supports * and ?)."
  (if (or (null mask) (string= mask ""))
      t
      (let* ((pattern (string-upcase mask))
             (target (string-upcase file-name))
             (plen (length pattern))
             (tlen (length target)))
        (labels ((match (pi ti)
                   (cond
                     ((= pi plen) (= ti tlen))
                     ((char= (char pattern pi) #\*)
                      (or (match (1+ pi) ti)
                          (and (< ti tlen) (match pi (1+ ti)))))
                     ((and (< ti tlen)
                           (or (char= (char pattern pi) #\?)
                               (char= (char pattern pi) (char target ti))))
                      (match (1+ pi) (1+ ti)))
                     (t nil))))
          (match 0 0)))))

(defclass directory-context ()
  ((path :initarg :path :accessor directory-context-path
         :documentation "Namestring of the directory being enumerated.")
   (entries :initarg :entries :accessor directory-context-entries
            :documentation "Vector of FILE-ENTRY-RECORD instances ready for traversal.")
   (position :initform 0 :accessor directory-context-position
             :documentation "Index of the next entry that should be returned.")
   (mask :initarg :mask :initform nil :accessor directory-context-mask
         :documentation "Original file mask used for enumeration.")
   (after-date :initarg :after-date :initform nil :accessor directory-context-after-date
               :documentation "Optional universal-time threshold for filtering uploads.")
   (audience :initarg :audience :initform :remote :accessor directory-context-audience
             :documentation "Either :SYSOP or :REMOTE depending on who requested the listing."))
  (:documentation "Holds state for iterating over a transfer directory."))

(defun %make-file-entry (pathname &key mask after-date)
  (let* ((file-name (file-namestring pathname)))
    (when (and file-name (file-ok-mask file-name mask))
      (let* ((length (%file-byte-length pathname))
             (write-date (ignore-errors (file-write-date pathname)))
             (mac-when (universal-time->mac-seconds write-date)))
        (when (or (null after-date)
                  (and mac-when (>= mac-when (universal-time->mac-seconds after-date))))
          (let* ((description (%truncate-string file-name 78))
                 (type (or (pathname-type pathname) ""))
                 (type-string (%truncate-string (string-upcase (or type "")) 4)))
            (make-instance 'file-entry-record
                           :fl-name (%truncate-string file-name 31)
                           :real-f-name (%truncate-string (namestring pathname) 255)
                           :fl-desc description
                           :when-ul (or mac-when 0)
                           :uploader-num 0
                           :num-dloads 0
                           :byte-len (or length 0)
                           :has-extended nil
                           :file-stat #\Space
                           :last-dl (or mac-when 0)
                           :version ""
                           :file-type type-string
                           :file-creator ""
                           :file-number 0))))))

(defun collect-directory-entries (path &key mask after-date audience)
  "Collect FILE-ENTRY-RECORD instances for PATH respecting MASK and AFTER-DATE."
  (declare (ignore audience))
  (let* ((base (%directory-pathname path))
         (pattern (%default-directory-pattern base))
         (paths (ignore-errors (directory pattern))))
    (loop for pathname in paths
          unless (%directory-pathname-p pathname)
            for entry = (%make-file-entry pathname :mask mask :after-date after-date)
            when entry collect entry)))

(defun generic-shell-sort (n compare exchange)
  "Shell-sort implementation that mimics the Pascal GenericShellSort routine."
  (when (> n 1)
    (let ((gap 1))
      (loop while (< gap n)
            do (setf gap (+ (* gap 3) 1)))
      (loop while (> gap 0)
            do (loop for i from gap below n
                     do (let ((j i))
                          (loop while (and (>= j gap)
                                           (funcall compare (- j gap) j))
                                do (funcall exchange (- j gap) j)
                                   (decf j gap))))
               (setf gap (floor gap 3)))))
  n)

(defun %entry-key-value (entry key)
  (ecase key
    (:name (string-upcase (file-entry-record-fl-name entry)))
    (:uploaded-at (file-entry-record-when-ul entry))
    (:downloads (file-entry-record-num-dloads entry))
    (:size (file-entry-record-byte-len entry))
    (:last-download (file-entry-record-last-dl entry))))

(defun sort-file-entries (entries key &key descending)
  "Return a fresh vector containing ENTRIES sorted by KEY using GenericShellSort."
  (let* ((vector (coerce entries 'vector))
         (length (length vector))
         (key (or key :name)))
    (labels ((needs-swap (left right)
               (let* ((lhs (%entry-key-value (aref vector left) key))
                      (rhs (%entry-key-value (aref vector right) key)))
                 (cond
                   ((and (stringp lhs) (stringp rhs))
                    (if descending
                        (string-lessp lhs rhs)
                        (string-lessp rhs lhs)))
                   (t
                    (let ((lhs (or lhs 0))
                          (rhs (or rhs 0)))
                      (if descending
                          (< lhs rhs)
                          (> lhs rhs)))))))
             (swap (left right)
               (rotatef (aref vector left) (aref vector right))))
      (generic-shell-sort length #'needs-swap #'swap))
    vector))

(defun open-directory (path &key mask sort-key descending after-date audience)
  "Open PATH for iteration returning a DIRECTORY-CONTEXT."
  (let* ((audience (or audience :remote))
         (entries (collect-directory-entries path :mask mask :after-date after-date :audience audience))
         (sorted (sort-file-entries entries (or sort-key :name) :descending descending))
         (path-string (or (ignore-errors (namestring (%directory-pathname path)))
                          (prin1-to-string path))))
    (with-hermes-log ()
      (format t "Opening directory ~A with ~D entries for ~A audience.~%"
              path-string (length sorted) audience))
    (make-instance 'directory-context
                   :path path-string
                   :entries sorted
                   :mask mask
                   :after-date after-date
                   :audience audience)))

(defun close-directory (context)
  "Release resources associated with CONTEXT."
  (when context
    (with-hermes-log ()
      (format t "Closing directory context for ~A.~%"
              (directory-context-path context)))
    (setf (directory-context-entries context) #()
          (directory-context-position context) 0))
  context)

(defun reset-directory (context)
  (setf (directory-context-position context) 0)
  context)

(defun directory-next-file (context)
  "Return the next FILE-ENTRY-RECORD from CONTEXT or NIL when exhausted."
  (let* ((entries (directory-context-entries context))
         (position (directory-context-position context))
         (length (length entries)))
    (when (< position length)
      (prog1 (aref entries position)
        (setf (directory-context-position context) (1+ position))))))

(defmacro with-directory ((var path &rest options) &body body)
  "Evaluate BODY with VAR bound to an open directory context."
  `(let ((,var (open-directory ,path ,@options)))
     (unwind-protect (progn ,@body)
       (close-directory ,var))))

(defun %directory-command (audience path &key mask sort-key descending after-date limit predicate)
  (with-directory (context path :mask mask :sort-key sort-key :descending descending
                           :after-date after-date :audience audience)
    (let ((results '())
          (count 0))
      (loop for entry = (directory-next-file context)
            while entry do
              (when (or (null predicate) (funcall predicate entry))
                (push entry results)
                (incf count)
                (when (and limit (>= count limit))
                  (return (nreverse results)))))
      (nreverse results))))

(defun sysop-directory-command (path &rest options)
  "Enumerate PATH using sysop visibility rules."
  (apply #'%directory-command :sysop path options))

(defun remote-directory-command (path &rest options)
  "Enumerate PATH using remote user visibility rules."
  (apply #'%directory-command :remote path options))
