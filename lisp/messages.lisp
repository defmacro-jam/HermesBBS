(in-package :hermes.messages)

(defconstant +message-block-size+ 512)
(defconstant +max-message-blocks+ 50)
(defconstant +max-index-count+ 15000)
(defparameter +mac-epoch-universal-time+ (encode-universal-time 0 0 0 1 1 1904 0))

(defstruct message-store
  (forum 0 :type integer)
  (conference 0 :type integer)
  (index (make-array 1 :initial-element 0 :adjustable t)
         :type vector)
  (index-path nil :type pathname)
  (text-path nil :type pathname))

(defun mac-time->universal-time (mac-seconds)
  (if (and mac-seconds (> mac-seconds 0))
      (+ +mac-epoch-universal-time+ mac-seconds)
      (get-universal-time)))

(defun mac-time->date-time (mac-seconds)
  (let ((universal (mac-time->universal-time mac-seconds)))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time universal)
      (values (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)
              (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))))

(defun ensure-directory-path (pathname)
  (let ((directory (make-pathname :defaults pathname :name nil :type nil)))
    (ensure-directories-exist directory)
    directory))

(defun colon-path->pathname (string)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Null) (or string "")))
         (converted (substitute #\/ #\: trimmed)))
    (cond
      ((zerop (length converted))
       (resolve-relative-path "Message/"))
      ((char= (char converted 0) #\/)
       (ensure-directory-path (pathname (if (char= (char converted (1- (length converted))) #\/)
                                           converted
                                           (concatenate 'string converted "/")))))
      (t
       (let* ((namestring (if (char= (char converted (1- (length converted))) #\/)
                              converted
                              (concatenate 'string converted "/")))
              (merged (merge-pathnames namestring (resolve-relative-path ""))))
         (ensure-directory-path merged))))))

(defun messages-root ()
  (let* ((system (gethash :system *legacy-caches*))
         (raw (and system (system-record-messages-path system))))
    (ensure-directory-path
     (if (and raw (> (length raw) 0))
         (colon-path->pathname raw)
         (resolve-relative-path "Message/")))))

(defun sanitize-component (name fallback)
  (let* ((trimmed (string-trim '(#\Space #\Null #\Return #\Linefeed)
                               (or name "")))
         (sanitized (map 'string
                         (lambda (ch)
                           (if (find ch "\\/:"
                                     :test #'char=)
                               #\-
                               ch))
                         trimmed)))
    (if (plusp (length sanitized))
        sanitized
        fallback)))

(defun fetch-forum-record (forum)
  (let* ((system (gethash :system *legacy-caches*))
         (limit (and system (system-record-message-forum-count system)))
         (table (gethash :message-forums *legacy-caches*)))
    (unless (and table limit (<= forum limit) (> forum 0))
      (error "Forum index ~D is invalid." forum))
    (let* ((forums (message-forum-table-forums table))
           (index (1- forum)))
      (unless (< index (length forums))
        (error "Forum index ~D is out of range." forum))
      (aref forums index)))

(defun fetch-conference-record (forum conference)
  (let ((tables (gethash :conference-tables *legacy-caches*)))
    (unless (> forum 0)
      (return-from fetch-conference-record nil))
    (let* ((table (and tables (nth (1- forum) tables))))
      (unless table
        (error "No conference table loaded for forum ~D." forum))
      (let* ((conferences (conference-table-conferences table))
             (index (1- conference)))
        (unless (and (>= conference 1) (< index (length conferences)))
          (error "Conference index ~D is invalid for forum ~D." conference forum))
        (aref conferences index)))))

(defun message-file-path (forum conference suffix)
  (let* ((root (messages-root))
         (relative
           (if (zerop forum)
               (format nil "Email/Email ~A" suffix)
               (let* ((forum-record (fetch-forum-record forum))
                      (conf-record (fetch-conference-record forum conference))
                      (forum-name (sanitize-component (forum-record-name forum-record)
                                                      (format nil "Forum-~D" forum)))
          (conf-name (sanitize-component (conference-record-name conf-record)
                                         (format nil "Conference-~D" conference))))
                 (format nil "~A/~A ~A" forum-name conf-name suffix)))))
    (let ((path (merge-pathnames relative root)))
      (ensure-directory-path path)
      path)))

(defun message-index-path (forum conference)
  (message-file-path forum conference "Indx"))

(defun message-text-path (forum conference)
  (message-file-path forum conference "Text"))

(defun ensure-index-bytes (vector)
  (let ((count (1- (length vector))))
    (let ((buffer (make-array (* count 2) :element-type '(unsigned-byte 8))))
      (dotimes (i count buffer)
        (let* ((value (aref vector (1+ i)))
               (encoded (mod value #x10000))
               (high (ldb (byte 8 8) encoded))
               (low (ldb (byte 8 0) encoded)))
          (setf (aref buffer (* i 2)) high)
          (setf (aref buffer (1+ (* i 2))) low))))))

(defun decode-index-bytes (bytes)
  (let* ((count (/ (length bytes) 2))
         (vector (make-array (1+ count) :initial-element 0 :adjustable t)))
    (dotimes (i count vector)
      (let* ((offset (* i 2))
             (high (aref bytes offset))
             (low (aref bytes (1+ offset)))
             (value (+ (ash high 8) low)))
        (when (> value 32767)
          (setf value (- value 65536)))
        (setf (aref vector (1+ i)) value)))))

(defun load-message-index (path)
  (if (probe-file path)
      (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
        (let* ((size (file-length stream))
               (buffer (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence buffer stream)
          (decode-index-bytes buffer)))
      (progn
        (ensure-directory-path path)
        (with-open-file (stream path :direction :output :if-does-not-exist :create
                                     :if-exists :supersede :element-type '(unsigned-byte 8))
          (declare (ignore stream)))
        (make-array 1 :initial-element 0 :adjustable t))))

(defun write-message-index (path vector)
  (let ((bytes (ensure-index-bytes vector)))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :element-type '(unsigned-byte 8))
      (write-sequence bytes stream)))
  vector)

(defun open-message-store (forum conference)
  (make-message-store :forum forum
                      :conference conference
                      :index (load-message-index (message-index-path forum conference))
                      :index-path (message-index-path forum conference)
                      :text-path (message-text-path forum conference)))

(defun %coerce-store (location conference)
  (etypecase location
    (message-store location)
    (integer (open-message-store location (or conference 0)))))

(defun %normalize-body (body)
  (etypecase body
    (string
    (let* ((normalized (substitute #\Return #\Newline body))
            (vector (make-array (length normalized)
                                 :element-type '(unsigned-byte 8))))
       (dotimes (i (length normalized) vector)
         (setf (aref vector i) (logand #xff (char-code (char normalized i)))))))
    ((simple-array (unsigned-byte 8) (*))
     (copy-seq body))
    ((vector (unsigned-byte 8))
     (coerce body '(simple-array (unsigned-byte 8) (*))))))

(defun %ensure-terminator (bytes)
  (let* ((length (length bytes))
         (needs-terminator (or (zerop length)
                               (/= (aref bytes (1- length)) 26))))
    (if needs-terminator
        (let ((extended (make-array (1+ length)
                                    :element-type '(unsigned-byte 8))))
          (replace extended bytes)
          (setf (aref extended length) 26)
          extended)
        bytes)))

(defun %allocate-blocks (index blocks-needed)
  (let* ((available (loop for idx from 1 below (length index)
                          when (zerop (aref index idx)) collect idx))
         (current-count (1- (length index))))
    (when (> blocks-needed +max-message-blocks+)
      (error "Message requires ~D blocks; limit is ~D." blocks-needed +max-message-blocks+))
    (when (< (length available) blocks-needed)
      (let* ((missing (- blocks-needed (length available)))
             (new-total (+ current-count missing)))
        (when (> new-total +max-index-count+)
          (error "Message store is full; cannot allocate ~D additional blocks." missing))
        (adjust-array index (1+ new-total) :initial-element 0)
        (setf available (nconc available
                               (loop for idx from (1+ current-count) to new-total collect idx)))) )
    (loop for block in available
          repeat blocks-needed
          collect block))

(defun append-message-body (body location &optional conference)
  (let* ((store (%coerce-store location conference))
         (index (message-store-index store))
         (bytes (%ensure-terminator (%normalize-body body)))
         (length (length bytes))
         (blocks-needed (ceiling length +message-block-size+))
         (blocks (%allocate-blocks index blocks-needed)))
    (with-open-file (stream (message-store-text-path store)
                            :direction :io
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (loop for block in blocks
            for offset from 0 by +message-block-size+
            for remaining = (max 0 (- length offset))
            for count = (min +message-block-size+ remaining)
            do (file-position stream (* (1- block) +message-block-size+))
               (write-sequence bytes stream :start offset :end (+ offset count))
               (when (< count +message-block-size+)
                 (let ((padding (make-array (- +message-block-size+ count)
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
                   (write-sequence padding stream)))))
    (loop for block in blocks
          for remainder on (append (rest blocks) (list -1))
          do (setf (aref index block) (if (first remainder) (first remainder) -1)))
    (write-message-index (message-store-index-path store) index)
    (values (first blocks) (length blocks)))

(defun %trim-message-bytes (bytes)
  (let ((terminator (position 26 bytes)))
    (cond
      (terminator (subseq bytes 0 terminator))
      (t (let ((end (length bytes)))
           (loop while (and (> end 0) (= (aref bytes (1- end)) 0)) do (decf end))
           (subseq bytes 0 end))))))

(defun %bytes->string (bytes)
  (let ((base (map 'string #'code-char bytes)))
    (let* ((normalized (substitute #\Newline #\Return base))
           (filtered (remove-if (lambda (ch)
                                  (and (< (char-code ch) 32)
                                       (not (member ch '(#\Newline #\Tab)))))
                                normalized)))
      filtered))

(defun read-message-body (stored-as location &optional conference &key (format :string))
  (when (<= (or stored-as 0) 0)
    (return-from read-message-body nil))
  (let* ((store (%coerce-store location conference))
         (index (message-store-index store))
         (limit (1- (length index))))
    (when (> stored-as limit)
      (return-from read-message-body nil))
    (with-open-file (stream (message-store-text-path store)
                            :direction :input
                            :if-does-not-exist nil
                            :element-type '(unsigned-byte 8))
      (unless stream
        (return-from read-message-body nil))
      (let* ((result (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0))
             (buffer (make-array +message-block-size+
                                  :element-type '(unsigned-byte 8)))
             (block stored-as))
        (loop while (and (> block 0) (<= block limit))
              do (file-position stream (* (1- block) +message-block-size+))
                 (let ((read (read-sequence buffer stream)))
                   (let ((start (fill-pointer result)))
                     (adjust-array result (+ start read))
                     (replace result buffer :start1 start :end1 (+ start read)
                                           :start2 0 :end2 read)))
                 (setf block (aref index block))
                 (when (< block 0) (setf block 0)))
        (let ((bytes (%trim-message-bytes (coerce result '(simple-array (unsigned-byte 8) (*))))))
          (ecase format
            (:bytes bytes)
            (:string (%bytes->string bytes))))))))

(defun delete-message-body (stored-as location &optional conference)
  (when (<= (or stored-as 0) 0)
    (return-from delete-message-body nil))
  (let* ((store (%coerce-store location conference))
         (index (message-store-index store))
         (limit (1- (length index))))
    (when (> stored-as limit)
      (return-from delete-message-body nil))
    (let ((block stored-as))
      (loop while (and (> block 0) (<= block limit))
            for next = (aref index block)
            do (setf (aref index block) 0)
               (setf block (if (< next 0) 0 next))))
    (write-message-index (message-store-index-path store) index)
    t))

(defun %split-lines (string)
  (let ((lines '())
        (start 0)
        (length (length string)))
    (loop for index from 0 to length
          do (when (or (= index length)
                       (char= (char string index) #\Newline))
               (push (subseq string start index) lines)
               (setf start (1+ index))))
    (nreverse lines)))

(defun %split-words (string)
  (let ((words '())
        (start 0)
        (in-word nil)
        (length (length string)))
    (labels ((finish (end)
               (when in-word
                 (push (subseq string start end) words)
                 (setf in-word nil))))
      (loop for index from 0 below length
            for ch = (char string index)
            do (if (find ch " \t" :test #'char=)
                   (finish index)
                   (unless in-word
                     (setf start index
                           in-word t))))
      (finish length))
    (nreverse words)))

(defun %derive-initials (name anonymous-p)
  (if anonymous-p
      "??> "
      (let* ((trimmed (string-trim '(#\Space #\Null) (or name "")))
             (parts (%split-words trimmed))
             (first-initial (if parts (char (string-upcase (first parts)) 0) #\?))
             (last-initial (if (> (length parts) 1)
                               (char (string-upcase (car (last parts))) 0)
                               first-initial)))
        (format nil "~C~C> " first-initial last-initial))))

(defun %replace-all (string target replacement)
  (with-output-to-string (out)
    (let* ((start 0)
           (target-length (length target))
           (limit (length string)))
      (loop for position = (search target string :start2 start)
            while position
            do (write-string string out :start start :end position)
               (write-string replacement out)
               (setf start (+ position target-length)))
      (when (< start limit)
        (write-string string out :start start :end limit)))))

(defun render-quote-header (message receiver)
  (let* ((system (gethash :system *legacy-caches*))
         (use-header (and system (system-record-use-quote-header system)))
         (anonymous (message-record-anonymous-from message)))
    (when use-header
      (let* ((options (system-record-quote-header-options system))
             (template (cond
                         ((and anonymous (= options 2)) nil)
                         ((and anonymous (= options 1)) (system-record-quote-header-anonymous system))
                         (t (system-record-quote-header system)))))
        (when (and template (> (length template) 0))
          (multiple-value-bind (date time)
              (mac-time->date-time (message-record-date-entered message))
            (let* ((sender (if anonymous "Anonymous" (message-record-from-user-name message)))
                   (target (or receiver (message-record-to-user-name message)))
                   (title (message-record-title message))
                   (header template))
              (setf header (%replace-all header "%sender" sender))
              (setf header (%replace-all header "%receiver" target))
              (setf header (%replace-all header "%date" date))
              (setf header (%replace-all header "%time" time))
              (setf header (%replace-all header "%title" title))
              header))))))

(defun %join-lines (lines)
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (write-char #\Newline out))
             (write-string line out))))

(defun quote-message (message &key receiver forum conference store prefix)
  (let* ((store (or store (when (or forum conference)
                            (open-message-store (or forum 0) (or conference 0)))))
         (body (read-message-body (message-record-stored-as message)
                                  (or store (or forum 0))
                                  (when (and (not store) forum) conference)
                                  :format :string)))
    (when (null body)
      (return-from quote-message nil))
    (let* ((clean body)
           (lines (%split-lines clean))
           (initials (or prefix
                         (%derive-initials (message-record-from-user-name message)
                                           (message-record-anonymous-from message))))
           (header (render-quote-header message receiver))
           (quoted (mapcar (lambda (line) (concatenate 'string initials line)) lines)))
      (if header
          (%join-lines (cons header quoted))
          (%join-lines quoted)))))

(defun post-ratio-ok-p (user)
  (let* ((levels (gethash :security-levels *legacy-caches*))
         (entry (and levels
                     (let* ((vector (security-level-table-levels levels))
                            (index (max 0 (1- (user-record-sl user)))))
                       (when (< index (length vector))
                         (aref vector index)))))
         (ratio (and entry (security-level-entry-post-ratio entry)))
         (messages (user-record-messages-posted user))
         (logons (max 1 (user-record-total-logons user))))
    (or (null ratio)
        (zerop ratio)
        (>= (/ (coerce messages 'double-float) logons)
            (/ 1.0d0 (coerce ratio 'double-float))))))

(defun attachments-directory (mail-p)
  (let* ((relative (if mail-p "Mail Attachments/" "Message Attachments/"))
         (path (merge-pathnames relative (messages-root))))
    (ensure-directory-path path)))

(defun delete-attachment (file-name &key mail-p)
  (let* ((directory (attachments-directory mail-p))
         (pattern (merge-pathnames (format nil "~A*" file-name) directory))
         (matches (directory pattern)))
    (if matches
        (progn
          (dolist (match matches)
            (ignore-errors (delete-file match)))
          t)
        nil)))
