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
  '()
  "List of packages to install")

;; refresh the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
