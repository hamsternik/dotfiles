;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; TBD to review gf3/dotfiles emacs config files
;;; https://github.com/gf3/dotfiles/blob/main/.config/emacs/init.el 

;;; Code: 

(setq custom-file "~/.emacs.custom.el")
;;(package-initialize)

;;; FIXME: Fira Code font does not work properly in Standalone Emacs 
;;; Check out any of provided workarounds: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions 
;;; (add-to-list (quote default-frame-alist (quote font . "Fira Code")))
;;; (add-to-list 'default-frame-alist `(font . "Iosevka"))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Ido (or "Interactively DO things)
;; (rc/require 'smex' 'ido-completing-read+)

;; scroll Emacs like lightning (macOS)
;;; https://github.com/jdtsmith/ultra-scroll
;;; based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

(load-file custom-file)
