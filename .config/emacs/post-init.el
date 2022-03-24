(defun cwchriswilliams/org-babel-auto-tangle()
 (when (string-equal (buffer-file-name)
		     (expand-file-name "~/D/I/arch_config/.config/emacs/post-init.org"))
	(org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cwchriswilliams/org-babel-auto-tangle)))

(setq backup-by-copying t
    backup-directory-alist
    '(("." . "~/.emacs-backups/"))
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2
    version-control t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-hl-line-mode +1)

(column-number-mode)

(global-display-line-numbers-mode t)
    (dolist (mode '(term-mode-hook
		  shell-mode-hook
		  eshell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook
	(lambda ()
	 (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(show-paren-mode 1)

(set-face-attribute 'default nil :font "Source Code Pro" :height 120)
