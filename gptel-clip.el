(require 'gptel)
(require 'cl-lib)

(defun my/gptel-context-to-clip ()
  "Collects all active gptel context, formats it into markdown chunks with
file/line metadata, and inserts it into a buffer named *clip*."
  (interactive)
  (let ((clip-buffer (get-buffer-create "*clip*"))
        (context-strings '()))
    
    ;; Iterate over the gptel-context alist
    ;; Structure is: ((#<buffer buf1> :overlays (#<overlay ...>)) ...)
    (dolist (entry gptel-context)
      (let* ((buf (car entry))
             (props (cdr entry))
             (overlays (plist-get props :overlays)))
        
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((source-name (or (buffer-file-name buf) (buffer-name buf))))
              
              ;; If there are overlays, process each one (Regions)
              (if overlays
                  (dolist (ov overlays)
                    (let ((start (overlay-start ov))
                          (end (overlay-end ov)))
                      (when (and start end) ;; Ensure overlay is valid
                        (let ((content (buffer-substring-no-properties start end))
                              (line-info (format "Lines %d-%d"
                                                 (line-number-at-pos start)
                                                 (line-number-at-pos end))))
                          (push (format "File: %s (%s)\n\n```\n%s\n```\n"
                                        source-name
                                        line-info
                                        content)
                                context-strings)))))
                
                ;; If no overlays, it implies the whole buffer is the context
                ;; (Note: gptel usually adds overlays even for whole buffers, 
                ;; but we handle the fallback just in case)
                (let ((content (buffer-substring-no-properties (point-min) (point-max)))
                      (line-info "Entire Buffer"))
                  (push (format "File: %s (%s)\n\n```\n%s\n```\n"
                                source-name
                                line-info
                                content)
                        context-strings))))))))

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
