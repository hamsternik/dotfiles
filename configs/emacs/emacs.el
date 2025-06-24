;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; - C-h f custom-file to see the function decl;
;;; - C-h v custom-file to see the variable decl and example of custom-file usage.
(setq custom-file "~/.emacs.custom.el")
(load custom-file) ;; OR (load-file custom-file)

(load-theme 'gruber-darker' t) ;; to load `gruber-darker` custom theme in emacs 24+

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-use-ls-dired nil)
;; (setq initial-buffer-choice t)
(setq initial-buffer-choice (lambda () (dired "~")))

(setq inhibit-startup-screen t) ;; to disable emacs splash screen
;; (setq initial-scratch-message nil) ;; to remove initial message in *scratch*

;;; FIXME: Fira Code font does not work properly in Standalone Emacs
;;; Check out any of provided workarounds: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;; (add-to-list (quote default-frame-alist (quote font . "Fira Code")))
;;; (add-to-list 'default-frame-alist `(font . "Iosevka"))

;; scroll Emacs like lightning (macOS)
;;; https://github.com/jdtsmith/ultra-scroll
;;; based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

(ido-mode 1) ;; enable ido-mode for better switching
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)


;;; Keybindings:
;;; emacs build-in Super key [`s`] equals Command in macOS.

(global-set-key [s-up] 'beginning-of-buffer)
(global-set-key [s-down] 'end-of-buffer)
(global-set-key (kbd "s-Z") 'undo-redo) ;; Super+Shift+z for emacs 28+ build-in `undo-redo`

;;; project / file navigation
(global-set-key (kbd "s-O") #'project-find-file) ;; to search a project-specific file like in Xcode
(global-set-key (kbd "s-b") #'switch-to-buffer) ;; to switch between emacs buffers

;;; emacs Buffers
(defun create-empty-buffer ()
    "Create a new empty buffer."
    (interactive)
    (let ((buf (generate-new-buffer "untitled")))
        (switch-to-buffer buf)))

(global-set-key (kbd "C-x C-e") #'eval-buffer) ;; rebind default `eval-last-sexp` with `eval-buffer` to evaluate the whole buffer at a time
(global-set-key (kbd "C-c C-e") 'eval-last-sexp) ;; bind `eval-last-sexp` to `ctrl-c ctrl-e` instead of default keybind
(global-set-key (kbd "C-c n") 'create-empty-buffer)

;;; split window
(global-set-key (kbd "s-\\") #'split-window-right) ;; to split the current window vertically, putting new window to the right
(global-set-key (kbd "s-|") #'split-window-below) ;; to split the current window horizontally, putting new window down below

;; try to move right, fallback to down
(defun hamsternik/windmove-right-or-down ()
  (interactive)
  (unless (ignore-errors (windmove-right)) (ignore-errors (windmove-down))))

(defun hamsternik/windmove-left-or-up ()
  (interactive)
  (unless (ignore-errors (windmove-left)) (ignore-errors (windmove-up))))

;; (windmove-default-keybindings) ;; to enable Shift+Arrow movement
(global-set-key (kbd "s-}") 'hamsternik/windmove-right-or-down) ;; to switch window to the *right* or *down* splitted one
(global-set-key (kbd "s-{") 'hamsternik/windmove-left-or-up) ;; to switch window to the *left* or *up* splitted one

;;; Packages:

;;; TBD to configure MELTA packages within gnu packages

;;; TBD to review video [Emacs: modern minibuffer packages](https://youtu.be/d3aaxOqwHhI)
