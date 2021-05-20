;;; rcoedo.el --- Personal utility library

;; Author: Roman Coedo
;; Created: 30 May 2017
;; Version: 0.2.0
;; Url: https://github.com/rcoedo/emacs-rcoedo/rcoedo.el
;; Keywords: utilities

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Utility function library

;;; Code:
(defun rcoedo-split-right-other-window ()
  "Split window right and move to that window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun rcoedo-split-below-other-window ()
  "Split window below and move to that window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun rcoedo-switch-to-buffer-right (buffer-or-name)
  "Split window right and then switch to buffer BUFFER-OR-NAME."
  (interactive)
  (rcoedo-split-right-other-window)
  (switch-to-buffer buffer-or-name))

(defun rcoedo-switch-to-buffer-below (buffer-or-name)
  "Split window right and then switch to buffer BUFFER-OR-NAME."
  (interactive)
  (rcoedo-split-below-other-window)
  (switch-to-buffer buffer-or-name))

(defun rcoedo-find-file-right (filename)
  "Split window right and then visit file FILENAME."
  (interactive)
  (rcoedo-split-right-other-window)
  (find-file filename))

(defun rcoedo-find-file-below (filename)
  "Split window below and then visit file FILENAME."
  (interactive)
  (rcoedo-split-below-other-window)
  (find-file filename))

(defun rcoedo-enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(defun rcoedo-rename-current-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun rcoedo-delete-current-file ()
  "Remove current buffer and its associated file."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rcoedo-emacs-buffer-p (name)
  "Return true if NAME match an Emacs buffer."
  (string-match-p "\\*.*\\*" name))

(defun rcoedo-next-non-emacs-buffer (&optional original)
  "Similar to 'next-buffer', but ignore emacs buffer such as *scratch*, *messages* etc."
  (interactive)
  (let ((tmp-orig (or original (buffer-name))))
    (next-buffer)
    (if (and
         (not (eq (buffer-name) tmp-orig))
         (rcoedo-emacs-buffer-p (buffer-name)))
        (rcoedo-next-non-emacs-buffer tmp-orig))))

(defun rcoedo-previous-non-emacs-buffer (&optional original)
  "Similar to previous-buffer, but ignore emacs buffer such as *scratch*, *messages* etc."
  (interactive)
  (let ((tmp-orig (or original (buffer-name))))
    (previous-buffer)
    (if (and
         (not (eq (buffer-name) tmp-orig))
         (rcoedo-emacs-buffer-p (buffer-name)))
        (rcoedo-previous-non-emacs-buffer tmp-orig))))

(defun rcoedo-eshell-maybe-bol ()
  "Move cursor to beginning of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun rcoedo-eshell-here ()
  "Open up a new shell in the dir associated with the current buffer file.
The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun rcoedo-eshell-prompt ()
  "Build a prompt string for eshell."
  (concat (getenv "USER") "@" (system-name) ":"
          (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(defun rcoedo-eshell-clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun rcoedo-projectile-eshell-popup ()
  "Open an eshell popup in the projectile root."
  (interactive)
  (let* ((height (/ (window-total-height) 3))
         (name   (projectile-project-root)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (cd (projectile-project-root)))
  (rcoedo-eshell-clear)
  (insert "ls")
  (eshell-send-input))

(defun rcoedo-helm-kill-buffers ()
  "Kill helm buffer candidates and stay open."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'helm-kill-buffers 'helm-kill-marked-buffers)
    (helm-execute-persistent-action 'helm-kill-buffers)
    (helm-force-update)))

(defun rcoedo-helm-switch-buffer-below ()
  "Run switch to buffer below action."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'rcoedo-switch-to-buffer-below)))

(defun rcoedo-helm-switch-buffer-right ()
  "Run switch to buffer right action."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'rcoedo-switch-to-buffer-right)))

(defun rcoedo-helm-find-file-below ()
  "Run find file below action."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'rcoedo-find-file-below)))

(defun rcoedo-helm-find-file-right ()
  "Run find file right action."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'rcoedo-find-file-right)))

(defun rcoedo-helm-ag-find-file-right ()
  "Split window right and run helm-ag--action-find-file."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'(lambda (args)
                                    (rcoedo-split-right-other-window)
                                    (funcall 'helm-ag--action-find-file args)))))

(defun rcoedo-helm-ag-find-file-below ()
  "Split window below and run helm-ag--action-find-file."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'(lambda (args)
                                    (rcoedo-split-below-other-window)
                                    (funcall 'helm-ag--action-find-file args)))))

(defun rcoedo-elixir-do-end-close-action (id action context)
  "Autoindent new elixir line using ID, ACTION and CONTEXT."
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(defun rcoedo-add-hooks (mode hooks)
  "Add mode MODE to hook list HOOKS."
  (dolist (hook hooks)
    (add-hook hook mode)))

(defun rcoedo-add-all-to-list (list-var elements)
  "Add to LIST-VAR all the ELEMENTS."
  (dolist (element elements)
          (add-to-list 'list-var element)
          (print list-var)))

(defun rcoedo-sort-words (reverse beg end)
  "Sort words from BEG to END in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
      (interactive "*P\nr")
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(provide 'rcoedo)
;;; rcoedo.el ends here
