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
  '(company smartparens flycheck editorconfig)
  "List of packages to install.")

(defconst theme-list
  '(dracula-theme spacemacs-theme)
  "List of themes to install.")

;; refresh the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; install missing themes
(dolist (theme theme-list)
  (unless (package-installed-p theme)
    (package-install theme)))

;; package-specific requirement(s)
(require 'smartparens-config)

;;; hook(s)
;; after initialization hook(s)
(defun after-init-configs ()
  "`after-init` specific set of configurations."
  ;; custom enabled minor mode(s)
  (global-company-mode))

;; configure `after-init'
(add-hook 'after-init-hook 'global-company-mode)

;; programming mode hook(s)
(defun prog-mode-configs ()
  "`prog-mode' specific set of configurations."
  ;; custom set variable(s)
  (setq show-trailing-whitespace t)
  (setq delete-trailing-lines t)

  ;; custom enabled minor mode(s)
  (smartparens-mode)
  (editorconfig-mode)
  (flycheck-mode))

;; configure `prog-mode'
(add-hook 'prog-mode-hook 'prog-mode-configs)

;;; themes
;; set the default theme
(load-theme 'spacemacs-dark t)

;; system-dependent config(s)
(cond ((eq system-type 'darwin)
       ;; mac-os configs
       ;; install exec-path-from-shell for OSX
       (unless (package-installed-p 'exec-path-from-shell)
	 (package-install 'exec-path-from-shell))

       ;; update $PATH environment for emacs
       (exec-path-from-shell-initialize)))
