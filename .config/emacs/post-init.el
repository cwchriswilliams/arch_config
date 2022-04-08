(defun cwchriswilliams/org-babel-auto-tangle()
  (when (or
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/emacs/post-init.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/i3/config.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/rofi/config.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/usr/share/X11/xorg.conf.d/41-libinput-user.org")))
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

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/pacakges/") t)

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))
(package-install-selected-packages)

(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
