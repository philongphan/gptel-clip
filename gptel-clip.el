(require 'gptel)
(require 'cl-lib)

;;; --- Configuration ---

(defgroup gptel-clip nil
  "Settings for gptel-clip."
  :group 'gptel)

(defcustom gptel-clip-buffer-name "*clip*"
  "Name of the buffer used for gptel context output."
  :type 'string
  :group 'gptel-clip)

(defconst gptel-clip--format-string "File: %s (%s)\n\n```\n%s\n```\n"
  "Format string for context entries. 
Args: File Name, Line Info, Content.")

;;; --- Internal Logic ---

(defun gptel-clip--format-entry (source-name start end content)
  "Format a single entry of code context."
  (let ((line-info (if (and start end)
                       (format "Lines %d-%d"
                               (line-number-at-pos start)
                               (line-number-at-pos end))
                     "Entire Buffer")))
    (format gptel-clip--format-string source-name line-info content)))

(defun gptel-clip--extract-buffer-context (buf props)
  "Extract formatted context strings from BUF based on PROPS."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((source-name (or (buffer-file-name buf) (buffer-name buf)))
            (overlays (plist-get props :overlays)))
        (if overlays
            ;; Case A: Specific Regions (Overlays)
            (cl-loop for ov in overlays
                     for start = (overlay-start ov)
                     for end = (overlay-end ov)
                     when (and start end)
                     collect (gptel-clip--format-entry 
                              source-name start end
                              (buffer-substring-no-properties start end)))
          ;; Case B: Entire Buffer
          (list (gptel-clip--format-entry 
                 source-name nil nil
                 (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun my/gptel--get-formatted-context-string ()
  "Helper function: Returns the formatted string of all gptel context."
  (mapconcat #'identity
             (cl-mapcan (lambda (entry)
                          (gptel-clip--extract-buffer-context (car entry) (cdr entry)))
                        gptel-context)
             "\n"))

;;; --- Commands ---

(defun my/gptel-context-to-clip ()
  "Collects gptel context, formats it, and inserts it into the clip buffer."
  (interactive)
  (let ((formatted-text (my/gptel--get-formatted-context-string))
        (clip-buffer (get-buffer-create gptel-clip-buffer-name)))
    
    (if (string-empty-p formatted-text)
        (message "No gptel context found.")
      (with-current-buffer clip-buffer
        (erase-buffer)
        (insert formatted-text)
        (if (fboundp 'markdown-mode)
            (markdown-mode)
          (text-mode))
        (goto-char (point-min)))
      (switch-to-buffer clip-buffer)
      (message "Context copied to %s" gptel-clip-buffer-name))))

(defun my/gptel-context-to-clipboard ()
  "Collects gptel context, formats it, and copies it to the system clipboard."
  (interactive)
  (let ((formatted-text (my/gptel--get-formatted-context-string)))
    (if (string-empty-p formatted-text)
        (message "No gptel context found.")
      (kill-new formatted-text)
      (message "Gptel context copied to clipboard! (%d chars)" (length formatted-text)))))

;;;###autoload
(defun gptel-clip-export ()
  "Export current gptel context (overlays + files) to *clip* as Markdown chunks."
  (interactive)
  (let* ((ctx (gptel-context--collect))
         (clip (gptel-clip--ensure-buffer)))
    (with-current-buffer clip
      (let ((inhibit-read-only t)
            (firstp t))
        (dolist (item ctx)
          (pcase item
            (`(,buf . ,ovs)
             (when (bufferp buf)
               ;; Stable order within a buffer: by position.
               (dolist (ov (sort (copy-sequence ovs)
                                 (lambda (a b)
                                   (< (or (overlay-start a) 0)
                                      (or (overlay-start b) 0)))))
                 (gptel-clip--insert-separator firstp)
                 (setq firstp nil)
                 (gptel-clip--insert-overlay-chunk buf ov))))
            (`(,path)
             (when (and (stringp path) (file-exists-p path))
               (gptel-clip--insert-separator firstp)
               (setq firstp nil)
               (gptel-clip--insert-file-chunk path)))
            (_
             ;; Ignore other context types (e.g. media) for this first version.
             nil)))))
    (pop-to-buffer clip)))

(provide 'gptel-clip)
;;; gptel-clip.el ends here.
