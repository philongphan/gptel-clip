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

;;; ── Fenced-block format helpers ──────────────────────────────────────────────

(defun gptel-patch--fence-p (s)
  (string-prefix-p "```" (string-trim-left s)))

(defun gptel-patch--file-path (s)
  (let ((s (string-trim s)))
    (when (string-match "\\`\\(?:#+\\s-*\\)?file:\\s-*\\(.+\\)\\'" s)
      (match-string 1 s))))
(defun gptel-patch--file-item (file text)
  (list :type 'file
        :file file
        :display file
        :text (replace-regexp-in-string "\n`+\\'" "" text)
        :save gptel-patch-save-file-targets))
(defun gptel-patch--parse-blocks (text)
  "Parse TEXT for fenced code blocks preceded by '# file: PATH' headers.
Returns a list of patch items with :type 'file."
  (let (items)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (gptel-patch--file-path
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))))
          (if (not path)
              (forward-line 1)
            (forward-line 1)
            (unless (gptel-patch--fence-p
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))
              (user-error "Found file: header without following fenced block"))
            (forward-line 1)
            (let ((block-start (point)))
              (unless (re-search-forward "^```" nil t)
                (user-error "Unclosed fenced code block in clipboard"))
              (push (gptel-patch--file-item
                     path
                     (buffer-substring-no-properties
                      block-start (line-beginning-position)))
                    items))))))
    (or (nreverse items)
        (user-error "No fenced file sections found in clipboard"))))
;;; ── Search/replace format helpers ────────────────────────────────────────────

(defconst gptel-patch--sr-search  "<<<<<<< SEARCH")
(defconst gptel-patch--sr-divider "=======")
(defconst gptel-patch--sr-replace ">>>>>>> REPLACE")

(defun gptel-patch--search-replace-p (text)
  "Return non-nil when TEXT contains at least one SEARCH/REPLACE block."
  (string-match-p (regexp-quote gptel-patch--sr-search) text))

(defun gptel-patch--parse-search-replace (text)
  "Parse all SEARCH/REPLACE blocks in TEXT.
Returns a list of (SEARCH-STRING . REPLACE-STRING) cons cells in order."
  (let (pairs)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward (concat gptel-patch--sr-search "\n") nil t)
        (let ((search-start (point)))
          (unless (search-forward (concat gptel-patch--sr-divider "\n") nil t)
            (user-error "gptel-patch: missing ======= in search/replace block"))
          (let ((search-text   (buffer-substring-no-properties
                                search-start (match-beginning 0)))
                (replace-start (point)))
            (unless (search-forward (concat gptel-patch--sr-replace) nil t)
              (user-error "gptel-patch: unclosed search/replace block (missing >>>>>>> REPLACE)"))
            ;; The REPLACE marker may or may not be followed by a newline.
            (let ((replace-text (buffer-substring-no-properties
                                 replace-start (match-beginning 0))))
              ;; Trim the trailing newline that precedes the marker line.
              (when (string-suffix-p "\n" replace-text)
                (setq replace-text (substring replace-text 0 -1)))
              (push (cons search-text replace-text) pairs))))))
    (or (nreverse pairs)
        (user-error "gptel-patch: no search/replace blocks found in clipboard"))))

(defun gptel-patch--apply-search-replace (text pairs)
  "Apply each (SEARCH . REPLACE) pair in PAIRS to TEXT in order.
Errors if any search string is not found."
  (dolist (pair pairs text)
    (let ((search  (car pair))
          (replace (cdr pair)))
      (unless (string-match-p (regexp-quote search) text)
        (user-error "gptel-patch: search text not found in buffer:\n%s"
                    (substring search 0 (min 120 (length search)))))
      (setq text (replace-regexp-in-string
                  (regexp-quote search) replace text
                  t   ; FIXEDCASE — don't adjust case of replacement
                  t   ; LITERAL   — treat replacement as plain string
                  )))))

;;; ── Core buffer/file utilities ───────────────────────────────────────────────

(defun gptel-patch--buffer-text (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-patch--buffer-display (buf)
  (with-current-buffer buf
    (or (buffer-file-name buf)
        (format "[buffer] %s" (buffer-name buf)))))

(defun gptel-patch--region-bounds (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (when (use-region-p)
      (cons (region-beginning) (region-end)))))

(defun gptel-patch--context-text (buf &optional bounds)
  (with-current-buffer buf
    (if bounds
        (buffer-substring-no-properties (car bounds) (cdr bounds))
      (gptel-patch--buffer-text buf))))

(defun gptel-patch--replace-region-text (buf text bounds)
  (with-current-buffer buf
    (concat (buffer-substring-no-properties (point-min) (car bounds))
            text
            (buffer-substring-no-properties (cdr bounds) (point-max)))))

(defun gptel-patch--buffer-item (buf text &optional save bounds)
  (list :type 'buffer
        :buffer buf
        :display (gptel-patch--buffer-display buf)
        :text (if bounds
                  (gptel-patch--replace-region-text buf text bounds)
                text)
        :save save))
(defun gptel-patch--replace-text (buf text)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))))

(defun gptel-patch--target-buffer (item)
  (pcase (plist-get item :type)
    ('buffer (plist-get item :buffer))
    ('file   (find-file-noselect (plist-get item :file)))))

(defun gptel-patch--cleanup (files)
  (dolist (f files)
    (when (and f (file-exists-p f))
      (ignore-errors (delete-file f)))))

;;; ── Review / approval UI ─────────────────────────────────────────────────────

(defun gptel-patch--show (item rest)
  (let* ((buf (gptel-patch--target-buffer item))
         (old (make-temp-file "old-"))
         (new (make-temp-file "new-")))
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
          (setq-local gptel-patch--current    item)
          (setq-local gptel-patch--remaining  rest)
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
  (let ((item  gptel-patch--current)
        (rest  gptel-patch--remaining)
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

;;; ── Entry point ──────────────────────────────────────────────────────────────

(defun gptel-patch ()
  "Replace the active region, or the current buffer, with raw clipboard text.
Clipboard content is treated as plain buffer text with no parsing."
  (interactive)
  (let* ((buf    (current-buffer))
         (bounds (gptel-patch--region-bounds buf)))
    (gptel-patch--show
     (gptel-patch--buffer-item buf
                               (gptel-patch--clipboard)
                               nil
                               bounds)
     nil)))
(defun gptel-patch-all ()
  "Apply fenced file blocks from the clipboard.
Each '# file: PATH' header must be followed by a fenced code block."
  (interactive)
  (let ((items (gptel-patch--parse-blocks (gptel-patch--clipboard))))
    (gptel-patch--show (car items) (cdr items))))

(defun gptel-patch-search-replace ()
  "Apply SEARCH/REPLACE blocks from the clipboard to the active region or current buffer."
  (interactive)
  (let* ((buf      (current-buffer))
         (bounds   (gptel-patch--region-bounds buf))
         (pairs    (gptel-patch--parse-search-replace
                    (gptel-patch--clipboard)))
         (new-text (gptel-patch--apply-search-replace
                    (gptel-patch--context-text buf bounds) pairs)))
    (gptel-patch--show
     (gptel-patch--buffer-item buf new-text nil bounds)
     nil)))
