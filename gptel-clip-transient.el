(require 'transient)
(require 'clip-5.4)

(defun gptel-clip-toggle-persist-task ()
  (interactive)
  (setq gptel-clip-persist-task (not gptel-clip-persist-task))
  (message "Task persistence %s"
           (if gptel-clip-persist-task "enabled" "disabled")))

(defun gptel-clip-toggle-include-project-file ()
  (interactive)
  (setq gptel-clip-include-project-file
        (not gptel-clip-include-project-file))
  (message "project.md preamble %s"
           (if gptel-clip-include-project-file "enabled" "disabled")))

(transient-define-prefix gptel-clip-transient ()
  "Dispatch gptel-clip commands."
  [["Flags"
    ("t" "Toggle task persistence" gptel-clip-toggle-persist-task :transient t)
    ("p" "Toggle project.md preamble" gptel-clip-toggle-include-project-file :transient t)]
   ["Actions"
    ("c" "Open clip buffer" gptel-clip)]])

(provide 'gptel-clip-transient)
