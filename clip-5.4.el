(defgroup gptel-clip nil
  "Helpers for exporting gptel context into a Markdown clip buffer."
  :group 'gptel)

(defcustom gptel-clip-persist-task t
  "When non-nil, preserve everything below \"# TASK\" in `*clip*` on refresh."
  :type 'boolean
  :group 'gptel-clip)

(defcustom gptel-clip-include-project-file t
  "When non-nil, prepend project.md from the current project to `*clip*`."
  :type 'boolean
  :group 'gptel-clip)

(defun gptel-clip--entry-get (data prop)
  (and (listp data) (keywordp (car data))
       (plist-get data prop)))

(defun gptel-clip--project-file ()
  (require 'project)
  (let* ((project (project-current nil))
         (root (and project (project-root project)))
         (file (and root (expand-file-name "project.md" root))))
    (and file (file-readable-p file) file)))

(defun gptel-clip--project-text ()
  (let ((project-file (gptel-clip--project-file)))
    (when project-file
      (with-temp-buffer
        (insert-file-contents project-file)
        (buffer-string)))))

(defun gptel-clip--sanitize-task-text (text)
  (let ((project-text (gptel-clip--project-text)))
    (if (and text project-text
             (string-match-p "[^[:space:]\n]" project-text))
        (replace-regexp-in-string
         (concat "\n*" (regexp-quote (string-trim-right project-text)) "\n*")
         "\n"
         text
         t t)
      text)))

(defun gptel-clip--insert-project-text ()
  (when gptel-clip-include-project-file
    (let ((project-text (gptel-clip--project-text)))
      (when (and project-text
                 (string-match-p "[^[:space:]\n]" project-text))
        (goto-char (point-min))
        (insert (string-trim-right project-text) "\n\n")
        t))))

(defun gptel-clip--extract-task-text (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "# TASK\n" nil t)
          (gptel-clip--sanitize-task-text
           (buffer-substring-no-properties (point) (point-max))))))))

(defun gptel-clip--insert-block (file body lang)
  (insert (format "## file: %s\n\n```%s\n%s" file lang body))
  (unless (bolp) (insert "\n"))
  (insert "```\n\n"))

(defun gptel-clip-copy-and-quit ()
  (interactive)
  (kill-new (buffer-string))
  (message "Copied *clip* to clipboard. Press C-y to yank in Emacs.")
  (quit-window t))

(defun gptel-clip--insert-style ()
  (insert "# STYLE\n\n"
          "- Write code changes as full files if file has <200 lines or multiple changes made on one file. Else indicate the code changes via search/replace blocks."
          "Especially use search/replace blocks when there is only little change on a file."
          "In general prefer using search/replace blocks."
          "Use `<<<<<<< SEARCH`, `=======` and `>>>>>>> REPLACE` as delimiters."
          "IMPORTANT: Each search/replace block MUST be enclosed in fenced code block with 4 backticks on each side.\n"
          "- Instead of explanation, write at the end a git commit message with what has been changed\n"
          "- For each file, you must add a file header above the code\n\n"))

(defun gptel-clip--insert-context (contexts)
  "Insert the CONTEXT section for CONTEXTS. Return non-nil if anything was rendered."
  (insert "# CONTEXT\n\n")
  (let (rendered)
    (pcase-dolist (`(,source . ,data) contexts)
      (cond
       ((bufferp source)
        (let* ((file (or (buffer-file-name source)
                         (format "[buffer] %s" (buffer-name source))))
               (lang (format "%s"
                             (gptel--strip-mode-suffix
                              (buffer-local-value 'major-mode source))))
               (ovs (seq-filter #'overlayp
                                (or (gptel-clip--entry-get data :overlays)
                                    data))))
          (if ovs
              (dolist (ov ovs)
                (setq rendered t)
                (gptel-clip--insert-block
                 file
                 (with-current-buffer source
                   (save-restriction
                     (widen)
                     (buffer-substring-no-properties
                      (overlay-start ov) (overlay-end ov))))
                 lang))
            (when (buffer-live-p source)
              (setq rendered t)
              (gptel-clip--insert-block
               file
               (with-current-buffer source
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties (point-min) (point-max))))
               lang)))))
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
    rendered))

(defun gptel-clip--insert-task (saved-task)
  (insert "# TASK\n")
  (insert (or saved-task "\n")))

(defun gptel-clip-add-file-to-context (file)
  (interactive "fAdd file to gptel context: ")
  (require 'gptel)
  (require 'gptel-context)
  (gptel-context-add-file (expand-file-name file))
  (message "Added %s to gptel context" (abbreviate-file-name file)))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "z") #'gptel-clip-add-file-to-context))

(defun gptel-clip ()
  (interactive)
  (require 'gptel)
  (require 'gptel-context)
  (require 'markdown-mode)
  (require 'subr-x)
  (let* ((contexts (gptel-context--collect))
         (buf (get-buffer-create "*clip*"))
         (saved-task (and gptel-clip-persist-task
                          (gptel-clip--extract-task-text buf)))
         rendered)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (markdown-mode)
        (gptel-clip--insert-project-text)
        (gptel-clip--insert-style)
        (setq rendered (gptel-clip--insert-context contexts))
        (gptel-clip--insert-task saved-task)
        (use-local-map (copy-keymap markdown-mode-map))
        (local-set-key (kbd "C-c C-c") #'gptel-clip-copy-and-quit)
        (outline-hide-sublevels 2)
        (goto-char (point-max))))
    (if rendered
        (progn
          (pop-to-buffer buf)
          (message "Updated *clip*. Press C-c C-c to copy to clipboard and close."))
      (message "No active gptel context right now."))))
