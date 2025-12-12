;;; gptel-clip.el --- Export gptel context into a Markdown *clip* buffer -*- lexical-binding: t; -*-

;; This file is independent glue code that reuses gptel's context collector.

(require 'gptel-context)
(require 'subr-x)
(require 'cl-lib)

(defgroup gptel-clip nil
  "Export gptel context into a Markdown buffer."
  :group 'gptel)

(defcustom gptel-clip-buffer-name "*clip*"
  "Name of the buffer that receives exported Markdown chunks."
  :type 'string
  :group 'gptel-clip)

(defcustom gptel-clip-clear-buffer t
  "If non-nil, overwrite the *clip* buffer on each export."
  :type 'boolean
  :group 'gptel-clip)

(defun gptel-clip--ensure-buffer ()
  "Return the clip buffer, created if necessary."
  (let ((buf (get-buffer-create gptel-clip-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'markdown-mode)
          (when (fboundp 'markdown-mode) (markdown-mode))))
      (when gptel-clip-clear-buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))
    buf))

(defun gptel-clip--insert-separator (firstp)
  (unless firstp
    (insert "\n\n")))

(defun gptel-clip--insert-overlay-chunk (buf ov)
  "Insert one chunk for overlay OV in BUF."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let* ((name (buffer-name buf))
             (beg (overlay-start ov))
             (end (overlay-end ov))
             (l1 (and beg (line-number-at-pos beg t)))
             (l2 (and end (line-number-at-pos end t)))
             (text (and beg end (buffer-substring-no-properties beg end))))
        (insert (format "### `%s` (lines %d-%d)\n\n" name (or l1 1) (or l2 1)))
        (insert "```
        (when text (insert text))
        (unless (or (null text) (string-suffix-p "\n" text))
          (insert "\n"))
        (insert "```\n")))))

(defun gptel-clip--insert-file-chunk (path)
  "Insert one chunk for file PATH."
  (let ((ident (file-name-nondirectory path)))
    (insert (format "### `%s`\n\n" ident))
    (insert "```
    (let ((pm (point-marker)))
      (set-marker-insertion-type pm t)
      (insert-file-contents path)
      (goto-char pm))
    (unless (bolp) (insert "\n"))
    (insert "```\n")))

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
