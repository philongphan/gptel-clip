(defgroup gptel-clip nil
  "Helpers for exporting gptel context into a Markdown clip buffer."
  :group 'gptel)

(defcustom gptel-clip-persist-task t
  "When non-nil, preserve everything below \"# TASK\" in `*clip*` on refresh."
  :type 'boolean
  :group 'gptel-clip)

(defun gptel-clip--context-overlays (entry-data)
  "Return overlays from ENTRY-DATA in either old or plist-shaped form."
  (cond
   ((null entry-data) nil)
   ((and (listp entry-data) (keywordp (car entry-data)))
    (plist-get entry-data :overlays))
   (t entry-data)))

(defun gptel-clip--context-mime (entry-data)
  "Return mime type from ENTRY-DATA if present."
  (when (and (listp entry-data) (keywordp (car entry-data)))
    (plist-get entry-data :mime)))

(defun gptel-clip--buffer-label (buffer)
  (or (buffer-file-name buffer)
      (format "[buffer] %s" (buffer-name buffer))))

(defun gptel-clip--buffer-lang (buffer)
  (format "%s"
          (gptel--strip-mode-suffix
           (buffer-local-value 'major-mode buffer))))

(defun gptel-clip--file-lang (path)
  (or (file-name-extension path) ""))

(defun gptel-clip--insert-fenced-block (header body lang)
  (insert (format "## file: %s\n\n" header))
  (insert (format "```%s\n" lang))
  (insert body)
  (unless (bolp) (insert "\n"))
  (insert "```\n\n"))

(defun gptel-clip--extract-task-text (buffer)
  "Return text below # TASK in BUFFER, or nil if not found."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "# TASK\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))))))

(defun gptel-clip-copy-and-quit ()
  "Copy current buffer contents and close its window."
  (interactive)
  (kill-new (buffer-string))
  (message "Copied *clip* to clipboard. Press C-y to yank in Emacs.")
  (quit-window t))

(defun gptel-clip ()
  "Collect gptel context and render it into *clip* as Markdown."
  (interactive)
  (require 'gptel)
  (require 'gptel-context)
  (require 'markdown-mode)
  (let* ((contexts (gptel-context--collect))
         (clip-buf (get-buffer-create "*clip*"))
         (saved-task (and gptel-clip-persist-task
                          (gptel-clip--extract-task-text clip-buf))))
    (with-current-buffer clip-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)

        (insert "# STYLE\n\n")
        (insert "# CONTEXT\n\n")

        (if (null contexts)
            (insert "_No active gptel context._\n\n")
          (pcase-dolist (`(,source . ,data) contexts)
            (cond
             ((bufferp source)
              (let ((ovs (gptel-clip--context-overlays data))
                    (label (gptel-clip--buffer-label source))
                    (lang (gptel-clip--buffer-lang source)))
                (dolist (ov ovs)
                  (gptel-clip--insert-fenced-block
                   label
                   (with-current-buffer source
                     (save-restriction
                       (widen)
                       (buffer-substring-no-properties
                        (overlay-start ov)
                        (overlay-end ov))))
                   lang))))

             ((stringp source)
              (unless (gptel-clip--context-mime data)
                (gptel-clip--insert-fenced-block
                 source
                 (with-temp-buffer
                   (insert-file-contents source)
                   (buffer-string))
                 (gptel-clip--file-lang source)))))))

        (insert "# TASK\n")
        (if saved-task
            (insert saved-task)
          (insert "\n"))

        (use-local-map (copy-keymap markdown-mode-map))
        (local-set-key (kbd "C-c C-c") #'gptel-clip-copy-and-quit)
        (goto-char (point-min))))
    (pop-to-buffer clip-buf)
    (message "Updated *clip*. Press C-c C-c to copy to clipboard and close.")))
