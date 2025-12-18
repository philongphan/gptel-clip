(require 'project)
(require 'ediff)
(require 'diff)

;;; --- Custom Ediff Commands ---

(defun llm-ediff-accept-all ()
  "Copy the entire content of Buffer B (LLM) to Buffer A (File)."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((buf-a ediff-buffer-A)
        (buf-b ediff-buffer-B))
    (when (y-or-n-p "Overwrite ENTIRE file with LLM content? ")
      (with-current-buffer buf-a
        (erase-buffer)
        (insert-buffer-substring buf-b))
      (ediff-recenter)
      (message "All changes applied. Don't forget to Save (C-x C-s in file buffer)."))))

(defun llm-ediff-setup-keys ()
  "Bind 'A' to accept all changes in Ediff."
  (define-key ediff-mode-map "A" 'llm-ediff-accept-all))

(add-hook 'ediff-keymap-setup-hook 'llm-ediff-setup-keys)

;;; --- Core Logic ---

(defun llm--start-ediff-session (file-buffer new-content)
  "Start Ediff. Buffer A = File, Buffer B = LLM Content."
  (let* ((file-name (buffer-file-name file-buffer))
         (base-name (file-name-nondirectory file-name))
         (proposal-buffer (generate-new-buffer (format "*LLM-Proposal: %s*" base-name))))
    
    ;; Setup Proposal Buffer
    (with-current-buffer proposal-buffer
      (insert new-content)
      (funcall (with-current-buffer file-buffer major-mode)))
    
    ;; Start Ediff
    (ediff-buffers 
     file-buffer 
     proposal-buffer
     `((lambda () 
         (add-hook 'ediff-quit-hook 
                   (lambda () 
                     (when (buffer-live-p ,proposal-buffer) 
                       (kill-buffer ,proposal-buffer)))
                   nil t))))
    (message "Ediff started. Press 'n'/'p' to navigate, 'b' to accept hunk, 'A' to ACCEPT ALL.")))

;;; --- Entry Points ---

(defun llm-apply-clipboard-to-current-buffer ()
  "Apply clipboard to current buffer via Diff.
Opens a simple diff buffer to view the changes.
When closing the diff buffer (with 'q'), the changes will be applied to the current file."
  (interactive)
  (let ((content (current-kill 0)))
    (if (string-empty-p content)
        (message "Clipboard is empty.")
      (let* ((file-buffer (current-buffer))
             (proposal-buffer (generate-new-buffer (format "*LLM-Proposal: %s*" (buffer-name file-buffer)))))
        
        ;; Setup Proposal Buffer
        (with-current-buffer proposal-buffer
          (insert content))
        
        ;; Start Diff
        (diff-buffers file-buffer proposal-buffer)
        
        ;; Configure Diff Buffer with Evil-compatible bindings
        (with-current-buffer "*Diff*"
          (setq-local header-line-format 
                      "Press 'A' to apply changes and close, 'q' to close without applying.")
          
          ;; Define the apply function
          (defun llm-diff-apply-and-close ()
            "Apply changes and close diff buffer."
            (interactive)
            (when (y-or-n-p "Apply changes to original buffer? ")
              (with-current-buffer file-buffer
                (erase-buffer)
                (insert-buffer-substring proposal-buffer)
                (message "Changes applied.")))
            (kill-buffer proposal-buffer)
            (quit-window t))
          
          ;; Set keybindings with high priority to override evil-mode
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "A") 'llm-diff-apply-and-close)
            (make-local-variable 'minor-mode-overriding-map-alist)
            (push `(t . ,map) minor-mode-overriding-map-alist))
          
          ;; Also try evil-specific approach if evil is loaded
          (when (bound-and-true-p evil-mode)
            (evil-local-set-key 'normal (kbd "A") 'llm-diff-apply-and-close)
            (evil-local-set-key 'motion (kbd "A") 'llm-diff-apply-and-close)))))))

(defun llm-apply-clipboard-to-ediff ()
  "Parse '### file:' blocks and Ediff them."
  (interactive)
  (let* ((clipboard (current-kill 0))
         (root (project-root (project-current t)))
         (regex "### file: \\(.*?\\)\n```[a-z]*\n\\(\\(?:.\\|\n\\)*?\\)```")
         (start 0)
         (found nil))
    
    (while (string-match regex clipboard start)
      (setq found t)
      (let* ((rel-path (string-trim (match-string 1 clipboard)))
             (new-content (match-string 2 clipboard))
             (abs-path (expand-file-name rel-path root)))
        
        (setq start (match-end 0))
        
        (if (file-exists-p abs-path)
            (llm--start-ediff-session (find-file-noselect abs-path) new-content)
          (if (y-or-n-p (format "File %s does not exist. Create it?" rel-path))
              (llm-create-new-file abs-path new-content)
            (message "Skipping %s" rel-path)))))
    
    (unless found
      (message "No '### file:' blocks found in clipboard."))))

(defun llm-create-new-file (path content)
  "Helper to create a fresh file."
  (let ((dir (file-name-directory path)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-current-buffer (find-file-noselect path)
    (insert content)
    (save-buffer))
  (message "Created %s" path))

(provide 'gptel-apply)
