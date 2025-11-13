(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DEFsystem"))

;; The system groups mirror Hermes.p units: initialization (Initial, CreateNewFiles,
;; LoadAndSave), preferences (NodePrefs*, SystPref*), networking (Terminal, Telnet,
;; TCPTypes, FileTrans*), UI (Message editors, Chatroom, Terminal), and utilities
;; (HUtils*, HermesUtils, Misc*).
(ccl:defsystem :hermes-bbs
  (:default-pathname #P"lisp/")
  (:components
   (:file "packages")
   (:file "utilities" :depends-on ("packages"))
   (:file "toolbox-wrappers" :depends-on ("utilities"))
   (:file "init" :depends-on ("utilities"))
   (:file "preferences" :depends-on ("utilities"))
   (:file "networking" :depends-on ("toolbox-wrappers" "utilities" "preferences"))
   (:file "ui" :depends-on ("toolbox-wrappers" "utilities"))
   (:file "main" :depends-on ("init" "preferences" "networking" "ui" "toolbox-wrappers" "utilities"))))

(defun load-hermes-bbs ()
  (ccl:load-system :hermes-bbs))

(export '(load-hermes-bbs))
