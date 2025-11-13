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
           #:user-record))

(defpackage :hermes.ui
  (:use :cl :ccl :hermes.common :hermes.toolbox :hermes.utilities)
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
           #:run-user-dialog-scenarios))

(defpackage :hermes.main
  (:use :cl :ccl :hermes.common :hermes.utilities :hermes.init
        :hermes.preferences :hermes.networking :hermes.ui :hermes.toolbox)
  (:export #:start-hermes
           #:request-hermes-shutdown))
