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

(setq backup-directory-alist `(("." . ,backup-path)))
(setq backup-by-copying t
      delete-old-versions t
      kept-old-versions 4
      kept-new-versions 4
      make-backup-files t
      version-control t)


;;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; init packages before modifying
(package-initialize)

;; install packages automatically if not already installed
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
    magit
    matlab-mode
    smartparens
    yaml-mode)
  "List of packages to install.")

(defconst theme-list
  '(dracula-theme spacemacs-theme)
  "List of themes to install.")

;; refresh the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; install missing themes
(dolist (theme theme-list)
  (unless (package-installed-p theme)
    (package-install theme)))

;; package-specific requirements
(require 'smartparens-config)

;;; Hooks
;; after initialization hooks
(defun after-init-configs ()
  "`after-init` specific set of configurations."
  ;; custom set variables

  ;; custom enabled minor modes
  (global-company-mode))

;; configure `after-init'
(add-hook 'after-init-hook 'global-company-mode)

;; text mode hooks
(defun text-mode-configs ()
  "`text-mode' specific set of configurations."
  ;; custom enabled minor modes
  (display-line-numbers-mode))

(add-hook 'text-mode-hook 'text-mode-configs)

;; programming mode hooks
(defun prog-mode-configs ()
  "`prog-mode' specific set of configurations."
  ;; custom set variables
  (setq show-trailing-whitespace t)
  (setq delete-trailing-lines t)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)

  ;; custom enabled minor modes
  (column-number-mode)
  (display-line-numbers-mode)
  (smartparens-mode)
  (editorconfig-mode)
  (flycheck-mode))

;; configure `prog-mode'
(add-hook 'prog-mode-hook 'prog-mode-configs)

;; c mode hooks
(defun c-mode-configs()
  "`c-mode' specific set of configurations."
  ;; custom set variables
  (setq indent-tabs-mode nil))

;; configure `c-mode'
(add-hook 'c-mode-hook 'c-mode-configs)

;; c++ mode hooks
(defun c++-mode-configs()
  "`c++-mode' specific set of configurations."

  ;; custom set variables
  (setq flycheck-gcc-language-standard "c++20"))

;; auctex mode hooks
(defun auctex-mode-configs()
  "`auctex-mode' specific set of configurations."
  ;; custom set variables
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq font-latex-fontify-sectioning 'color)

  (setq-default TeX-master "main")

  ;; custom enabled minor modes
  (turn-on-reftex)
  (flyspell-mode))

;; configure `auctex-mode'
(add-hook 'LaTeX-mode-hook 'auctex-mode-configs)

;;; Themes
;; set the default theme
(load-theme 'spacemacs-dark t)

;; system-dependent configs
(cond ((eq system-type 'darwin)
       ;; mac-os configs
       ;; install exec-path-from-shell for OSX
       (unless (package-installed-p 'exec-path-from-shell)
         (package-install 'exec-path-from-shell))

       ;; update $PATH environment for emacs
       (exec-path-from-shell-initialize)))
