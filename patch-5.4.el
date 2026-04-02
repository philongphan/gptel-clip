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

(defun gptel-patch--parse-blocks (text)
  "Parse TEXT for fenced code blocks preceded by '# file: PATH' headers.
Returns a list of patch items with :type 'file."
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
  "Apply clipboard content as a patch to one or more buffers/files.

Three clipboard formats are recognised, tried in order:

1. SEARCH/REPLACE blocks (Aider-style) — applied to the current buffer:

     <<<<<<< SEARCH
     exact text to find
     =======
     replacement text
     >>>>>>> REPLACE

   Multiple blocks are applied in sequence; the combined result is shown
   as a single unified diff for review.

2. Fenced code blocks preceded by '# file: PATH' headers — each block
   replaces the entire content of the named file:

     # file: src/foo.el
     ```emacs-lisp
     ;; full new content
     ```

   Multiple file sections are queued and reviewed one at a time.

3. Raw text (no fences, no SEARCH markers) — replaces the entire current
   buffer, shown as a diff for review."
  (interactive)
  (let* ((text (gptel-patch--clipboard)))
    (cond
     ;; ── Format 1: search/replace blocks ──
     ((gptel-patch--search-replace-p text)
      (let* ((pairs    (gptel-patch--parse-search-replace text))
             (buf      (current-buffer))
             (new-text (gptel-patch--apply-search-replace
                        (gptel-patch--buffer-text buf) pairs))
             (item     (list :type    'buffer
                             :buffer  buf
                             :display (or (buffer-file-name buf)
                                          (format "[buffer] %s" (buffer-name buf)))
                             :text    new-text
                             :save    nil)))
        (gptel-patch--show item nil)))
     ;; ── Format 2: fenced file blocks ──
     ((string-match-p "```" text)
      (let ((items (gptel-patch--parse-blocks text)))
        (gptel-patch--show (car items) (cdr items))))
     ;; ── Format 3: raw text → replace current buffer ──
     (t
      (gptel-patch--show
       (list :type    'buffer
             :buffer  (current-buffer)
             :display (or (buffer-file-name)
                          (format "[buffer] %s" (buffer-name)))
             :text    text
             :save    nil)
       nil)))))
