(in-package :cl-user)

(defpackage :hermes.common
  (:use :cl :ccl)
  (:export #:*hermes-root*
           #:resolve-relative-path
           #:with-hermes-log
           #:timestamp-string
           #:*event-timeout*
           #:define-background-task
           #:start-background-task
           #:stop-background-task
           #:*background-processes*
           #:ensure-hermes-state
           #:hermes-state
           #:hermes-state-startup-time
           #:hermes-state-resource-files
           #:hermes-state-shutdown-hooks
           #:hermes-state-global-seed
           #:hermes-state-random-state
           #:hermes-state-network-ready-p
           #:hermes-state-running-p
           #:*hermes-state*
           #:update-hermes-state))

(defpackage :hermes.utilities
  (:use :cl :ccl :hermes.common)
  (:export #:initialize-logging
           #:seed-global-state
           #:format-datestamp
           #:register-shutdown-hook
           #:run-shutdown-hooks))

(defpackage :hermes.init
  (:use :cl :ccl :hermes.common :hermes.utilities)
  (:export #:bootstrap-initial-files
           #:initialize-databases
           #:prime-message-stores))

(defpackage :hermes.preferences
  (:use :cl :ccl :hermes.common :hermes.utilities)
  (:export #:load-node-preferences
           #:load-system-preferences
           #:persist-preferences))

(defpackage :hermes.toolbox
  (:use :cl :ccl :hermes.common)
  (:export #:process-info
           #:process-serial-number
           #:process-type
           #:process-name
           #:list-processes
           #:find-process-by-name
           #:with-apple-event-handler
           #:dispatch-apple-event
           #:make-apple-event
           #:install-apple-event-handlers
           #:ensure-mactcp-started
           #:open-tcp-stream
           #:close-tcp-stream
           #:with-tcp-stream
           #:register-scheduled-task
           #:cancel-scheduled-task
           #:tcp-session
           #:tcp-session-host
           #:tcp-session-port
           #:tcp-session-stream
           #:tcp-session-driver))

(defpackage :hermes.networking
  (:use :cl :ccl :hermes.common :hermes.toolbox :hermes.utilities)
  (:export #:initialize-networking
           #:start-telnet-service
           #:start-modem-service
           #:shutdown-networking))

(defpackage :hermes.storage
  (:use :cl :ccl :hermes.common)
  (:export #:pascal-layout
           #:record-byte-size
           #:serialize-record
           #:deserialize-record
           #:with-legacy-file
           #:read-legacy-records
           #:write-legacy-record
           #:read-resource-record
           #:write-resource-record
           #:*legacy-caches*
           #:warm-legacy-caches
           #:user-record
           #:system-record
           #:forum-record
           #:conference-record
           #:message-record
           #:email-record
           #:message-forum-table
           #:conference-table
           #:security-level-entry
           #:security-level-table
           #:file-entry-record
           #:file-entry-record-fl-name
           #:file-entry-record-real-f-name
           #:file-entry-record-fl-desc
           #:file-entry-record-when-ul
           #:file-entry-record-uploader-num
           #:file-entry-record-num-dloads
           #:file-entry-record-byte-len
           #:file-entry-record-has-extended
           #:file-entry-record-file-stat
           #:file-entry-record-last-dl
           #:file-entry-record-version
           #:file-entry-record-file-type
           #:file-entry-record-file-creator
           #:file-entry-record-file-number))

(defpackage :hermes.file-areas
  (:use :cl :ccl :hermes.common :hermes.storage :hermes.utilities)
  (:export #:generic-shell-sort
           #:sort-file-entries
           #:file-ok-mask
           #:directory-context
           #:directory-context-path
           #:directory-context-entries
           #:directory-context-position
           #:directory-context-mask
           #:directory-context-after-date
           #:directory-context-audience
           #:open-directory
           #:close-directory
           #:directory-next-file
           #:reset-directory
           #:with-directory
           #:collect-directory-entries
           #:sysop-directory-command
           #:remote-directory-command))

(defpackage :hermes.messages
  (:use :cl :ccl :hermes.common :hermes.utilities)
  (:import-from :hermes.storage
                #:*legacy-caches*
                #:system-record
                #:system-record-messages-path
                #:system-record-message-forum-count
                #:system-record-use-quote-header
                #:system-record-quote-header
                #:system-record-quote-header-anonymous
                #:system-record-quote-header-options
                #:message-forum-table
                #:message-forum-table-forums
                #:forum-record
                #:forum-record-name
                #:conference-table
                #:conference-table-conferences
                #:conference-record
                #:conference-record-name
                #:message-record
                #:message-record-title
                #:message-record-from-user-name
                #:message-record-to-user-name
                #:message-record-anonymous-from
                #:message-record-stored-as
                #:message-record-date-entered
                #:email-record
                #:security-level-table
                #:security-level-table-levels
                #:security-level-entry
                #:security-level-entry-post-ratio
                #:user-record
                #:user-record-sl
                #:user-record-messages-posted
                #:user-record-total-logons)
  (:export #:open-message-store
           #:read-message-body
           #:append-message-body
           #:delete-message-body
           #:quote-message
           #:post-ratio-ok-p
           #:delete-attachment))

(defpackage :hermes.ui
  (:use :cl :ccl :hermes.common :hermes.toolbox :hermes.utilities :hermes.messages :hermes.file-areas)
  (:import-from :hermes.storage
                #:user-record
                #:serialize-record
                #:deserialize-record)
  (:export #:initialize-ui
           #:display-splash
           #:refresh-session-windows
           #:make-user-editor-dialog
           #:make-user-list-dialog
           #:make-user-search-dialog
           #:find-control
           #:handle-dialog-event
           #:compile-search-criteria
           #:filter-user-list
           #:run-user-dialog-scenarios
           #:ansi-terminal-view
           #:make-ansi-terminal-view
           #:reset-terminal
           #:write-terminal-character
           #:write-terminal-string
           #:terminal-selection-text
           #:set-terminal-palette
           #:set-terminal-font
           #:terminal-buffer-contents))

(defpackage :hermes.tests
  (:use :cl :ccl :hermes.common :hermes.utilities :hermes.storage :hermes.networking :hermes.messages)
  (:export #:workflow-event
           #:workflow-event-type
           #:workflow-event-payload
           #:make-workflow-event
           #:workflow-script
           #:workflow-script-name
           #:workflow-script-events
           #:make-workflow-script
           #:workflow-context
           #:make-workflow-context
           #:workflow-context-state
           #:workflow-context-transcript
           #:workflow-transcript-entries
           #:register-workflow-handler
           #:execute-workflow-script
           #:make-login-event
           #:make-message-post-event
           #:make-telnet-negotiation-event
           #:make-core-workflow-script
           #:run-core-workflow-script
           #:run-core-workflows))

(defpackage :hermes.main
  (:use :cl :ccl :hermes.common :hermes.utilities :hermes.init
        :hermes.preferences :hermes.networking :hermes.ui :hermes.toolbox :hermes.messages)
  (:export #:start-hermes
           #:request-hermes-shutdown))
