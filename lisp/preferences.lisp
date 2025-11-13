(in-package :hermes.preferences)

(defun load-node-preferences ()
  (with-hermes-log ()
    (format t "Loading node preferences (NodePrefs, NodePrefs2).~%"))
  (update-hermes-state :resource-file :node-prefs))

(defun load-system-preferences ()
  (with-hermes-log ()
    (format t "Loading system preferences (SystPref, SystPrefs2).~%"))
  (update-hermes-state :resource-file :system-prefs))

(defun persist-preferences ()
  (with-hermes-log ()
    (format t "Persisting Hermes preferences to disk.~%")))
