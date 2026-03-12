(require 'diff)
(require 'subr-x)

(defgroup gptel-patch nil "" :group 'gptel)

(defcustom gptel-patch-save-file-targets t
  ""
  :type 'boolean
  :group 'gptel-patch)

(defvar-local gptel-patch--current nil)
(defvar-local gptel-patch--remaining nil)
(defvar-local gptel-patch--temp-files nil)

(defun gptel-patch--clipboard ()
  (or (and (fboundp 'gui-get-selection)
           (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
               (gui-get-selection 'CLIPBOARD 'STRING)))
      (current-kill 0 t)
      (user-error "Clipboard is empty")))

(defun gptel-patch--fence-p (s)
  (string-prefix-p "```" (string-trim-left s)))

(defun gptel-patch--file-path (s)
  (let ((s (string-trim s)))
    (when (string-match "\\`\\(?:#+\\s-*\\)?file:\\s-*\\(.+\\)\\'" s)
      (match-string 1 s))))

(defun gptel-patch--parse-blocks (text)
  (let (items pending current inside lines)
    (dolist (line (split-string text "\n"))
      (if inside
          (if (gptel-patch--fence-p line)
              (progn
                (push (list :type 'file
                            :file current
                            :display current
                            :text (string-join (nreverse lines) "\n")
                            :save gptel-patch-save-file-targets)
                      items)
                (setq inside nil current nil lines nil))
            (push line lines))
        (let ((path (gptel-patch--file-path line)))
          (cond
           (path
            (setq pending path))
           ((gptel-patch--fence-p line)
            (unless pending
              (user-error "Found fenced block without preceding file: header"))
            (setq inside t current pending pending nil lines nil))
           ((string-blank-p line))
           (t
            (setq pending nil))))))
    (when inside
      (user-error "Unclosed fenced code block in clipboard"))
    (or (nreverse items)
        (user-error "No fenced file sections found in clipboard"))))

(defun gptel-patch--buffer-text (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-patch--replace-text (buf text)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))))

(defun gptel-patch--target-buffer (item)
  (pcase (plist-get item :type)
    ('buffer (plist-get item :buffer))
    ('file (find-file-noselect (plist-get item :file)))))

(defun gptel-patch--cleanup (files)
  (dolist (f files)
    (when (and f (file-exists-p f))
      (ignore-errors (delete-file f)))))

(defun gptel-patch--show (item rest)
  (let* ((buf (gptel-patch--target-buffer item))
         (old (make-temp-file "gptel-patch-old-"))
         (new (make-temp-file "gptel-patch-new-")))
    (with-temp-file old (insert (gptel-patch--buffer-text buf)))
    (with-temp-file new (insert (plist-get item :text)))
    (let ((dbuf (diff-no-select old new "-u" 'noasync)))
      (if (not dbuf)
          (progn
            (gptel-patch--cleanup (list old new))
            (if rest
                (gptel-patch--show (car rest) (cdr rest))
              (message "gptel-patch: no changes")))
        (with-current-buffer dbuf
          (rename-buffer (format "*gptel-patch: %s*" (plist-get item :display)) t)
          (setq-local gptel-patch--current item)
          (setq-local gptel-patch--remaining rest)
          (setq-local gptel-patch--temp-files (list old new))
          (setq header-line-format "gptel-patch: C-c C-c apply, C-c C-k skip")
          (use-local-map (copy-keymap diff-mode-map))
          (local-set-key (kbd "C-c C-c") #'gptel-patch-approve)
          (local-set-key (kbd "C-c C-k") #'gptel-patch-reject))
        (pop-to-buffer dbuf)
        (message "Review patch for %s. C-c C-c applies, C-c C-k skips."
                 (plist-get item :display))))))

(defun gptel-patch--finish (apply)
  (unless gptel-patch--current
    (user-error "No active gptel-patch review"))
  (let ((item gptel-patch--current)
        (rest gptel-patch--remaining)
        (temps gptel-patch--temp-files))
    (gptel-patch--cleanup temps)
    (when apply
      (let ((buf (gptel-patch--target-buffer item)))
        (gptel-patch--replace-text buf (plist-get item :text))
        (when (and (plist-get item :save) (buffer-file-name buf))
          (with-current-buffer buf
            (save-buffer)))))
    (quit-window t)
    (if rest
        (gptel-patch--show (car rest) (cdr rest))
      (message "gptel-patch: %s %s"
               (if apply "applied" "skipped")
               (plist-get item :display)))))

(defun gptel-patch-approve ()
  (interactive)
  (gptel-patch--finish t))

(defun gptel-patch-reject ()
  (interactive)
  (gptel-patch--finish nil))

(defun gptel-patch ()
  (interactive)
  (let* ((text (gptel-patch--clipboard))
         (items (if (string-match-p "```" text)
                    (gptel-patch--parse-blocks text)
                  (list (list :type 'buffer
                              :buffer (current-buffer)
                              :display (or (buffer-file-name)
                                           (format "[buffer] %s" (buffer-name)))
                              :text text
                              :save nil)))))
    (gptel-patch--show (car items) (cdr items))))
