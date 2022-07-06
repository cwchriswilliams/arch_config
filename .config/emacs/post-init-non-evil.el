(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

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

(tab-bar-mode 1)
(tab-bar-history-mode 1)

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
  :keymaps 'override
  :prefix "S-SPC")
 (general-create-definer personal/refactor
  :keymaps 'override
  :prefix "M-RET"))

(use-package treemacs)

(use-package org
  :custom (org-ellipsis " ➤")
  (org-log-done 'time)
  (org-agenda-start-with-log-mode t))

(use-package org-bullets
:after org
:hook (org-mode . org-bullets-mode))

(use-package magit
 :custom (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package treemacs-magit)

(use-package forge :after magit)

(use-package magit-gitflow
  :hook 'magit-mode-hook (turn-on-magit-gitflow))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode t))

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

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(general-def
   "C-'" 'avy-goto-char-timer
   "C-S-f" 'swiper
   "C-S-p" 'counsel-M-x
[remap describe-function] 'counsel-describe-function
[remap descibe-command] 'helpful-command
[remap describe-variable] 'counsel-describe-variable
[remap describe-key] 'helpful-key)

(use-package company
 :demand
 :config (global-company-mode)
 :general ("C-S-SPC" 'company-complete))

(use-package projectile
 :demand
 :general ("C-c p" 'projectile-command-map)
 :init (when (file-directory-p "~/D/I")
	 (setq projectile-project-search-path '("~/D/I")))
 :config (projectile-mode +1))

(use-package ripgrep :demand)

(use-package projectile-ripgrep :after projectile ripgrep)

(use-package counsel-projectile :after projectile :config (counsel-projectile-mode t))

(use-package treemacs-projectile :after projectile)

(personal/leader-key
  "p" 'projectile-command-map
  "ps" '(:ignore t :which-key "search"))

(use-package rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
 :hook (prog-mode . enable-paredit-mode))

(use-package lsp-mode
 :init (setq lsp-keymap-prefix "C-C l")
 :custom (lsp-lens-enable t)
 :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(use-package flycheck)

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package dash
  :after yasnippet)

(use-package ivy-yasnippet
  :after yasnippet dash
  :config 'ivy-yasnippet)

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo))

(add-hook 'clojure-mode 'lsp)
(add-hook 'clojurescript-mode 'lsp)
(add-hook 'clojurec-mode 'lsp)

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode)
	 (clojurec-mode . clj-refactor-mode)
	 (clojurescript-mode . clj-refactor-mode))
 :config (cljr-add-keybindings-with-prefix "M-RET"))

(use-package flycheck-clj-kondo)

(use-package clojure-snippets
  :after yasnippet clojure-mode)

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-eval-toplevel-inside-comment-form t)
  (clojure-toplevel-inside-comment-form t))

;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(personal/leader-key
  "'" '(:ignore t :which-key "cider")
  "'j" '(:ignore t :which-key "jack-in")
  "'jj" 'cider-jack-in-clj
  "'js" 'cider-jack-in-cljs
  "'jc" 'cider-jack-in-clj&cljs

  "s" 'sesman-map

  "l" '(:ignore t :which-key "portal")
  "lo" #'portal.api/open
  "lc" #'portal.api/clear

  "h" '(:ignore t :which-key "doc")
  "hd" 'cider-doc
  "hj" 'cider-javadoc
  "hc" 'cider-clojure-docs
  "ha" 'cider-apropos
  "hA" 'cider-apropos-documentation
  "hw" 'cider-clojuredocs-web
  "hn" 'cider-browse-ns

  "e" '(:ignore t :which-key "eval")
  "ee" 'cider-eval-defun-at-point
  "ef" 'cider-eval-last-sexp
  "eb" 'cider-eval-buffer
  "ec" 'cider-eval-commands-map)

(general-def "C-<return>" 'cider-eval-defun-at-point)

(personal/refactor
 "a" '(:ignore t :which-key "add")
 "c" '(:ignore t :which-key "cycle")
 "d" '(:ignore t :which-key "destructure")
 "e" '(:ignore t :which-key "extract/expand")
 "f" '(:ignore t :which-key "function")
 "h" '(:ignore t :which-key "hydra/hotload")
 "i" '(:ignore t :which-key "introduce/inline")
 "m" '(:ignore t :which-key "move")
 "p" '(:ignore t :which-key "project/promote")
 "r" '(:ignore t :which-key "rename/remove")
 "s" '(:ignore t :which-key "sort/stop deps")
 "t" '(:ignore t :which-key "thread")
 "u" '(:ignore t :which-key "unwind"))

(personal/leader-key "i" 'ivy-yasnippet)

(use-package nov
 :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(general-def
 "C-S-f" 'projectile-ripgrep)

(general-def
  "M-Z" 'zap-up-to-char
  "M-i" 'imenu)
