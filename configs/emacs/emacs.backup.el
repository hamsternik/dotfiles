(setq custom-file "~/.emacs.custom.el")

(setq initial-buffer-choice "~") ;; Open this file on Emacs startup
;;(setq initial-buffer-choice (lambda () (dired "~")))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode t)

;; disable the splash screen
(setq inhibit-startup-screen t)

;; remove the initial scratch message
(setq initial-scratch-message nil)

;; load custom theme
(load-theme 'gruber-darker' t)

;;; FIXME: Fira Code font does not work properly in Standalone Emacs 
;;; Check out any of provided workarounds: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions 
;;; (add-to-list (quote default-frame-alist (quote font . "Fira Code")))
;;; (add-to-list 'default-frame-alist `(font . "Iosevka"))

;;; ido (interactively do things)
;(use-package smex
;  :ensure t)

;(use-package ido-completing-read+
;  :ensure t)

(ignore-errors
  (require 'smex)
  (require 'ido-completing-read+))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; scroll Emacs like lightning (macOS)
;;; https://github.com/jdtsmith/ultra-scroll
;;; based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

(load-file custom-file)

(use-package vterm
  :ensure t)

(require 'package)

;; add MELPA to the list of repositories
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "htpps://melpa.org/packages/")))
;; initialize the package system
(package-initialize)
;; optional: refresh package list if it is empty
(unless package-archive-contents
  (package-refresh-contents))
