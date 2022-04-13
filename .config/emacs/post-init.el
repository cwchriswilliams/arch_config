(defun cwchriswilliams/org-babel-auto-tangle()
  (when (or
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/emacs/post-init.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/i3/config.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/rofi/config.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/usr/share/X11/xorg.conf.d/41-libinput-user.org"))
	 (string-equal (buffer-file-name)
		       (expand-file-name "~/D/I/arch_config/.config/i3blocks/config.org"))
	 )
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
(menu-bar-mode -1)

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

(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

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

(use-package gruvbox-theme
 :config (load-theme 'gruvbox-dark-medium t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package which-key
 :custom (which-key-idle-delay 0.3)
 :config (which-key-mode))

(use-package general
 :demand
 :config
 (general-create-definer personal/leader-key
  :states '(normal visual insert motion)
  :prefix "C-SPC"))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
 :after evil
 :config (evil-collection-init))

(use-package magit
 :custom (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(personal/leader-key
 "g" '(:ignore t :which-key "git")
 "gs" 'magit-status)

(use-package avy)

(use-package counsel
:config (counsel-mode 1)
        (ivy-mode 1))

(use-package ivy-rich
:config (ivy-rich-mode 1))

(use-package helpful
  :custom
(counsel-describe-function-function #'helpful-callable)
(counsel-describe-variable-function #'helpful-variable))

(general-def '(normal insert visual motion)
   "C-'" 'avy-goto-char-timer
   "C-f" 'swiper
   "C-S-p" 'counsel-M-x
[remap describe-function] 'counsel-describe-function
[remap descibe-command] 'helpful-command
[remap describe-variable] 'counsel-describe-variable
[remap describe-key] 'helpful-key)

(general-def '(motion normal insert visual)
 "C-z" 'undo
 "C-S-z" 'undo-redo
 "C-s" 'save-buffer
 "C-S-c" 'clipboard-kill-ring-save
 "C-S-v" 'clipboard-yank
 "C-S-x" 'clipboard-kill-region
 "C-<tab>" 'switch-to-buffer)
