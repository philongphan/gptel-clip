(require 'gptel)
(require 'cl-lib)
(require 'eieio)

(defun my/gptel-context-to-clip ()
  "Collects all active gptel context, formats it into markdown chunks with
file/line metadata, and inserts it into a buffer named *clip*."
  (interactive)
  (let ((clip-buffer (get-buffer-create "*clip*"))
        (context-strings '()))
    
    ;; Iterate over each item in gptel's context list
    (dolist (item gptel-context)
      (let (source-name line-info content)
        
        ;; Determine the type of context and extract info
        (cond
         ;; Case 1: Region (Overlay)
         ((and (object-of-class-p item 'gptel-context-region)
               (slot-boundp item 'overlay))
          (let* ((ov (oref item overlay))
                 (buf (overlay-buffer ov)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (let ((start (overlay-start ov))
                      (end (overlay-end ov)))
                  (setq source-name (or (buffer-file-name buf) (buffer-name buf))
                        line-info (format "Lines %d-%d"
                                          (line-number-at-pos start)
                                          (line-number-at-pos end))
                        content (buffer-substring-no-properties start end)))))))

         ;; Case 2: Buffer
         ((and (object-of-class-p item 'gptel-context-buffer)
               (slot-boundp item 'buffer))
          (let ((buf (get-buffer (oref item buffer))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq source-name (or (buffer-file-name) (buffer-name))
                      line-info "Entire Buffer"
                      content (buffer-substring-no-properties (point-min) (point-max)))))))

         ;; Case 3: File
         ((and (object-of-class-p item 'gptel-context-file)
               (slot-boundp item 'path))
          (let ((path (oref item path)))
            (when (file-exists-p path)
              (setq source-name path
                    line-info "Entire File"
                    content (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string)))))))
        
        ;; Format the chunk if we successfully extracted content
        (when (and source-name content)
          (push (format "File: %s (%s)\n\n```\n%s\n```\n"
                        source-name
                        line-info
                        content)
                context-strings))))

    ;; Insert everything into the *clip* buffer
    (with-current-buffer clip-buffer
      (erase-buffer)
      (insert (mapconcat #'identity (nreverse context-strings) "\n"))
      (if (fboundp 'markdown-mode)
          (markdown-mode)
        (text-mode))
      (goto-char (point-min)))
    
    (switch-to-buffer clip-buffer)
    (message "Context copied to *clip*")))

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
