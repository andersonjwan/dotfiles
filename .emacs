;; emacs configuration file
;; @author: Jacob Anderson <andersonjwan@outlook.com>

;; disable the tool bar, menu bar, and scroll bar
;; to reduce clutter of the editor if a windowing system is
;; in use (i.e. not a terminal interface)
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; init packages before modifying
(package-initialize)

;; install packages automatically if not preset
;; list of packages
(defconst package-list
  '(company smartparens flycheck editorconfig dracula-theme)
  "List of packages to install.")

;; refresh the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; package-specific requirement(s)
(require 'smartparens-config)

;;; hook(s)
;; after initialization hook(s)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'editorconfig-mode)

;; programming mode hook(s)
(add-hook 'prog-mode-hook 'smartparens-mode)

;; custom set variable(s)
(setq-default show-trailing-whitespace t
	      delete-trailing-lines t)

;;; themes
;; set the default theme
(load-theme 'dracula t)
