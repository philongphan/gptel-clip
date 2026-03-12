(defgroup gptel-clip nil
  "Helpers for exporting gptel context into a Markdown clip buffer."
  :group 'gptel)

(defcustom gptel-clip-persist-task t
  "When non-nil, preserve everything below \"# TASK\" in `*clip*` on refresh."
  :type 'boolean
  :group 'gptel-clip)

(defun gptel-clip--entry-get (data prop)
  (and (listp data) (keywordp (car data))
       (plist-get data prop)))

(defun gptel-clip--extract-task-text (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "# TASK\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))))))

(defun gptel-clip--insert-block (file body lang)
  (insert (format "## file: %s\n\n```%s\n%s" file lang body))
  (unless (bolp) (insert "\n"))
  (insert "```\n\n"))

(defun gptel-clip-copy-and-quit ()
  (interactive)
  (kill-new (buffer-string))
  (message "Copied *clip* to clipboard. Press C-y to yank in Emacs.")
  (quit-window t))

(defun gptel-clip ()
  (interactive)
  (require 'gptel)
  (require 'gptel-context)
  (require 'markdown-mode)
  (let* ((contexts (gptel-context--collect))
         (buf (get-buffer-create "*clip*"))
         (saved-task (and gptel-clip-persist-task
                          (gptel-clip--extract-task-text buf)))
         rendered)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)
        (insert "# STYLE\n\n")
        (insert "- Return code of full files\n")
        (insert "- For each file, you must add a file header above the code \n\n")
        (insert "# CONTEXT\n\n")
        (pcase-dolist (`(,source . ,data) contexts)
          (cond
           ((bufferp source)
            (let ((file (or (buffer-file-name source)
                            (format "[buffer] %s" (buffer-name source))))
                  (lang (format "%s"
                                (gptel--strip-mode-suffix
                                 (buffer-local-value 'major-mode source))))
                  (ovs (seq-filter #'overlayp
                                   (or (gptel-clip--entry-get data :overlays) data))))
              (dolist (ov ovs)
                (setq rendered t)
                (gptel-clip--insert-block
                 file
                 (with-current-buffer source
                   (save-restriction
                     (widen)
                     (buffer-substring-no-properties
                      (overlay-start ov) (overlay-end ov))))
                 lang))))
           ((and (stringp source)
                 (not (gptel-clip--entry-get data :mime)))
            (setq rendered t)
            (gptel-clip--insert-block
             source
             (with-temp-buffer
               (insert-file-contents source)
               (buffer-string))
             (or (file-name-extension source) "")))))
        (unless rendered
          (insert "_No active gptel context._\n\n"))
        (insert "# TASK\n")
        (insert (or saved-task "\n"))
        (use-local-map (copy-keymap markdown-mode-map))
        (local-set-key (kbd "C-c C-c") #'gptel-clip-copy-and-quit)
        (goto-char (point-min))))
    (if rendered
        (progn
          (pop-to-buffer buf)
          (message "Updated *clip*. Press C-c C-c to copy to clipboard and close."))
      (message "No active gptel context right now."))))
