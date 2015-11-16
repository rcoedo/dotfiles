(defun eshell-maybe-bol ()
      (interactive)
      (let ((p (point)))
        (eshell-bol)
        (if (= p (point))
            (beginning-of-line))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
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

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell-projectile ()
  (interactive)
  (let* ((height (/ (window-total-height) 3))
         (name   (projectile-project-root)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (cd (projectile-project-root)))
  (eshell/clear)
  (insert "ls")
  (eshell-send-input))

(defun eshell/x ()
  (kill-buffer-and-window))
