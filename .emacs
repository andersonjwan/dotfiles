;;; package --- My custom Emacs configurations.
;;; Commentary:
;;; Emacs configuration file.
;;; @author: Jacob Anderson <andersonjwan@gmail.com>

;;; Code:
;; disable the tool bar, menu bar, and scroll bar
;; to reduce clutter of the editor
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;;; Backups
;; For more information, see:
;; https://stackoverflow.com/a/18330742
(defvar backup-path (concat user-emacs-directory "backups"))
(if (not (file-directory-p backup-path))
    (make-directory backup-path t))

(setq backup-directory-alist '(("." . backup-path)))
(setq backup-by-copying t
      delete-old-versions t
      kept-old-versions 4
      kept-new-versions 4
      make-backup-files t
      version-control t)


;;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; init packages before modifying
(package-initialize)

;; install packages automatically if not preset
;; list of packages
(defconst package-list
  '(auctex
    cmake-mode
    company
    company-reftex
    cython-mode
    dockerfile-mode
    editorconfig
    flycheck
    matlab-mode
    smartparens
    yaml-mode)
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
  ;; custom set variable(s)

  ;; custom enabled minor mode(s)
  (global-company-mode))

;; configure `after-init'
(add-hook 'after-init-hook 'global-company-mode)

;; text mode hook(s)
(defun text-mode-configs ()
  "`text-mode' specific set of configurations."
  ;; custom enabled minor mode(s)
  (display-line-numbers-mode))

(add-hook 'text-mode-hook 'text-mode-configs)

;; programming mode hook(s)
(defun prog-mode-configs ()
  "`prog-mode' specific set of configurations."
  ;; custom set variable(s)
  (setq show-trailing-whitespace t)
  (setq delete-trailing-lines t)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)

  ;; custom enabled minor mode(s)
  (column-number-mode)
  (display-line-numbers-mode)
  (smartparens-mode)
  (editorconfig-mode)
  (flycheck-mode))

;; configure `prog-mode'
(add-hook 'prog-mode-hook 'prog-mode-configs)

;; c mode hook(s)
(defun c-mode-configs()
  "`c-mode' specific set of configurations."
  ;; custom set variable(s)
  (setq indent-tabs-mode nil))

;; configure `c-mode'
(add-hook 'c-mode-hook 'c-mode-configs)

;; c++ mode hook(s)
(defun c++-mode-configs()
  "`c++-mode' specific set of configurations."

  ;; custom set variable(s)
  (setq flycheck-gcc-language-standard "c++20"))

;; auctex mode hook(s)
(defun auctex-mode-configs()
  "`auctex-mode' specific set of configurations."
  ;; custom set variable(s)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq font-latex-fontify-sectioning 'color)

  (setq-default TeX-master "main")

  ;; custom enabled minor mode(s)
  (turn-on-reftex)
  (flyspell-mode))

;; configure `auctex-mode'
(add-hook 'LaTeX-mode-hook 'auctex-mode-configs)

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
