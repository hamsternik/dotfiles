;;  -*- coding: utf-8; lexical-binding: t -*- 

;;; Intro:
;; See https://github.com/patrickt/emacs for inspiration.

;;; Commentary:
;;; - C-h f custom-file to see the function decl;
;;; - C-h v custom-file to see the variable decl and example of custom-file usage.

(use-package emacs
  :ensure nil
  :custom
  (when (window-system)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (menu-bar-mode 0))
  (inhibit-startup-screen t)
  (indent-tabs-mode nil) ;; disable the use of tabs for indentation (spaces instead)
  (tab-always-indent 'complete) ;; make the TAB key complete text instead of just indenting
  (tab-width 4)
  (display-line-numbers-type 'absolute)
  (delete-selection-mode 1)
  (truncate-lines t)
  (use-short-answers t)

  :hook
  (prog-mode . display-line-numbers-mode))


(when (eq system-type 'darwin)
  ;; make Commnad key act as Meta
  (setq mac-command-modifier 'meta)
  ;; keep Option/Alt as Meta
  (setq mac-option-modifier 'meta)
  ;; make Control stay as Control
  (setq mac-control-modifier 'control)
  ;; optional: make right Command act as Super instead
  (setq mac-right-command-modifier 'super))

;;; package initialization first
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; set custom filepath to keep all nongnu/melpa plugins:
;; macOS/Darwin: ~/.config/emacs/custom.init.el
;; Linux/WSL: ~/.emacs.d/custom.init.el
(setq custom-file
      (if (eq system-type 'darwin)
          "~/.config/emacs/custom.init.el"
        "~/.emacs.d/custom.init.el"))
(load custom-file)

;; (load-theme 'gruber-darker' t)

;;; Modus Themes
;;; https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :ensure nil
  :demand t
  :config
  (setq mode-line-compact nil)
  (setq modus-themes-common-palette-overrides
	'((bg-mode-line-active bg-blue-subtle)
	  (fg-mode-line-active fg-main)
	  (border-mode-line-active bg-blue-subtle)
	  (border-mode-line-inactive bg-dim)
	  (bg-region bg-hl-line) ;; OR alternative 'bg-lavender color
	  (fg-region unsepcified)))

  ;; Finally, load your theme of choice:
  ;; - modus-operandi, the light theme
  ;; - modus-vivdendi, the dark theme
  ;; OR switch between them via: M-x modus-themes-toggle

  ;; Load theme 
  (load-theme 'modus-vivendi :no-confirm))

;;; set up a visible bell instead of audio
;;; FIXME: to turn on **only** on non-macOS. macOS manages visiable bell w/ the weird huge yellow triangle in the middle of the screen.
(setq visible-bell t)

;; restore the last emacs session, including the buffer for the file, *scratch* buffer, etc
(desktop-save-mode 1)
;; enables `auto-revert-mode` globally, makes emacs automatically reload files if they are modified outside of emacs
(global-auto-revert-mode 1)
;; [!NOTE]: This code does not work for the *scratch* buffer specifically. Need to store *scratch* buffer content manually to a file.
;;(setq desktop-buffers-not-to-save (delete "\\*scratch\\*" desktop-buffers-not-to-save))

;;; FIXME: Fira Code font does not work properly in Standalone Emacs
;;; TBD to check out workaround here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;(set-face-attribute 'default nil
;;		    :font "Fira Code-14")
;;(set-fontset-font t 'latin "Iosevka-14" nil 'append)
;;(set-fontset-font t 'latin "Roboto Mono-14" nil 'append)

;; Font platform-specific platform configuration
(cond
 ;; macOS: use preferred fonts: Fira Code, Iosevka, Roboto Mono
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Fira Code-14")
  (set-fontset-font t 'latin "Iosevka-14" nil 'append)
  (set-fontset-font t 'latin "Roboto Mono-14" nil 'append))
;; otherwise, try Fira Code, fallback to monospace
(t
 (if (find-font (font-spec :name "Fira Code"))
 (set-face-attribute 'default nil :font "Fira Code-14")
 (set-face-attribute 'default nil :font "monospace-14"))))

;; scroll Emacs like lightning (macOS)
;;; https://github.com/jdtsmith/ultra-scroll
;;; based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

(defvar hn/scratch-file (expand-file-name "scratch.txt" user-emacs-directory))
(defun hn/save-scratch ()
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max) hn/scratch-file)))
(defun hn/restore-scratch ()
  (when (file-exists-p hn/scratch-file)
    (with-current-buffer "*scratch*"
      (delete-region (point-min) (point-max))
      (insert-file-contents hn/scratch-file))))
(add-hook 'kill-emacs-hook 'hn/save-scratch)
(add-hook 'after-init-hook 'hn/restore-scratch)

;; disable auto-save files in emacs
;; (setq auto-save-default nil)

;; keeping auto-save files enabled but moving files to a central dir:
(make-directory (expand-file-name "auto-save/" user-emacs-directory) t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;;; IDO:
;;; Interactively Do Things (ido-mode)
;;; turning on `fido-vertical-mode` (emacs 28+) instead of old-fashion `ido-mode` 
(fido-vertical-mode 1)

;; Keybindings:
;;; emacs build-in Super key [`s`] equals Command in macOS.
;;; =======================================================

;;; macOS Keybindings:
;;; based on https://www.emacswiki.org/emacs/EmacsForMacOS%20
(when (eq system-type 'darwin)
  ;;  (setq mac-option-modifier 'alt)
  ;;  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char))

(global-set-key [s-up] 'beginning-of-buffer)
(global-set-key [s-down] 'end-of-buffer)
(global-set-key (kbd "s-Z") 'undo-redo) ;; Super+Shift+z for emacs 28+ build-in `undo-redo`

(defun delete-to-start-of-line ()
  "Delete all content from the cursor to the start of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(global-set-key (kbd "s-<backspace>") 'delete-to-start-of-line)

;;; reload emacs config
(defun reload-emacs-config ()
  "Reload emacs.el Emacs configuration file"
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-r") 'reload-emacs-config) ;; C-c C-r to reload

;;; project / file navigation
(global-set-key (kbd "s-O") #'project-find-file) ;; to search a project-specific file like in Xcode
(global-set-key (kbd "s-S") #'switch-to-buffer) ;; to switch between emacs buffers

;;; emacs *scratch* buffer
(global-set-key (kbd "C-c s SPC") (lambda () (interactive) (switch-to-buffer "*scratch*")))

;;; comment line or region
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)

(add-hook 'text-mode-hook 'outline-minor-mode)
(add-hook 'fundamental-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; M-! key bind alternatives
(bind-key "C-;" 'shell-command)

;; Buffers:
;;; Custom functions and emacs window keybindings
;;; =============================================

(global-set-key (kbd "C-c o") 'buffer-menu)

;; create new empty *untitled* buffer
(defun create-empty-buffer () 
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)))

(global-set-key (kbd "C-x C-e") #'eval-buffer) ;; rebind default `eval-last-sexp` with `eval-buffer` to evaluate the whole buffer at a time
(global-set-key (kbd "C-c C-e") 'eval-last-sexp) ;; bind `eval-last-sexp` to `ctrl-c ctrl-e` instead of default keybind
(global-set-key (kbd "C-c n") 'create-empty-buffer)
(global-set-key (kbd "C-c r") 'rename-buffer)

;; re-indent the entire emacs buffer
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c i") 'indent-buffer) ;; bind to fix the indentation as Xcode standard keybind

;;; Windows:
;;; Custom functions and emacs window keybindings
;;; =============================================

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

;; close the 2nd (split) window in the buffer
(defun close-current-window ()
  "Close the currently active split window."
  (interactive)
  (if (not (one-window-p))
      (delete-window)
    (message "Cannot close the only window")))

(global-set-key (kbd "s-w") 'close-current-window)


;;; Packages:
;;; =========

;;;; Built-in packages:
;;;; ==================

;;; Dired
;;;; the directory editor
;;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
(use-package dired
  :ensure nil ; built-in, no need to install
  :bind (:map dired-mode-map) (";" . shell-command)
  :bind (:map dired-mode-map) ("-" . dired-up-directory))

(setq dired-use-ls-dired nil)
;; (setq initial-buffer-choice t)
(setq initial-buffer-choice (lambda () (dired "~")))

;; !TIP: use `g` to refresh the buffer to see the latest changed in the dir
;; auto-refresh dired buffers when files change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; Project.el
(use-package project
  :ensure nil)

;;; ansi-color
;;;; enable ANSI color support in compilation buffers
;;;; based on https://github.com/anschwa/emacs.d?tab=readme-ov-file#ansi-color-codes
;;;; FIXME: broken ansi symbols in standard fish greeting message when run `C-x p s` shell
(use-package ansi-color
  :ensure nil  ;; built-in package
  :hook
  ;; Modren shell-mode ANSI color support (Emacs 28+)
  (shell-mode . hn/setup-shell-ansi-color)
  (compilation-filter . hn/colorize-compilation-buffer)
  :config
  (defun hn/setup-shell-ansi-color ()
    "Setup modern ANSI color handling for shell-mode."
    ;; Remove the old processor if present
    (setq-local comint-output-filter-functions
		(remove 'ansi-color-process-output comint-output-filter-functions))
    ;; Add the new region-based processor
    (add-hook 'comint-output-filter-functions
	      #'ansi-color-apply-on-region nil t))
  
  (defun hn/ansi-color (&optional beg end)
    "Interpret ANSI color escape sequence by colorifying content.
Operate on selected region or whole buffer."
    (interactive
     (if (use-region-p)
	 (list (region-beginning) (region-end))
       (list (point-min) (point-max))))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region beg end)))

  (defun hn/colorize-compilation-buffer ()
    "Colorize compilation buffer by interpreting ANSI color codes."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))  
  )

;;; LSP mode

;; swiftlang/sourcekit-lsp: language protocol impl. for Swift and C-based lang.
;; https://github.com/joaotavora/eglot/issues/825#issuecomment-1024267560
;; FIXME: no code completion in .swift file
(defun hamsternik/sourcekit-lsp-executable ()
  (setq hamsternik/sourcekit-lsp-executable
	(cond ((executable-find "sourcekit-lsp"))
	      ((equal system-type 'darwin)
	       (cond ((executable-find "/Library/Developer/CommandLineTools/usr/bin/sourcekit-lsp"))))
	      ((equal system-type 'gnu/linux)
	       (cond ((executable-find "/home/linuxbrew/.linuxbrew/bin/sourcekit-lsp"))))
	      (t
	       ("sourcekit-lsp")))))
(defun hamsternik/sourcekit-lsp-command (interactive)
  (append (list (hamsternik/sourcekit-lsp-executable))))

;;;; !NOTE:
;;; lsp-mode vs. lsp-bridge vs. lspce vs. eglot
;;; discussion on reddit: https://www.reddit.com/r/emacs/comments/1c0v28k/lspmode_vs_lspbridge_vs_lspce_vs_eglot/

;;;; eglot
;;; https://github.com/joaotavora/eglot
;;; a client for LSP servers; built-in since Emacs 29
(use-package eglot
  :ensure nil
  ;; :defer t
  ;; :hook ((python-mode . eglot-ensure))
  ;; :custom
  ;; (eglot-report-progress nil)  ; Prevent minibuffer spam
  :bind
  (:map eglot-mode-map
	("C-." . 'xref-find-definitions)
	("C-," . 'xref-go-back)
	("C-c ?" . 'eglot-help-at-point)
	("C-c C-c" . 'eglot-code-actions)
	("C-c C-r" . 'eglot-rename))
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '((swift-mode) . hamsternik/sourcekit-lsp-command)))

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell"))

;;;; Installed packages:
;;;; ===================


;; set a reasonable default PATH
;;; !WARNING: config execution takes 0.745ms
;;(use-package exec-path-from-shell
;;  :ensure t
;;  :config
;;  (exec-path-from-shell-initialize))

;;;; https://github.com/dbordak/telephone-line
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

;;; magit.el
;;;; https://github.com/magit/magit
(use-package magit
  :ensure t)

;;;; TBD to read about Essential Settings
;;;;; https://docs.magit.vc/magit/Essential-Settings.html


;;; https://github.com/jrblevin/markdown-mode
;;;; TBD to verify whether `aspell` is installed
;;;; macOS: brew install aspell
;;;; Win/WSL: sudo apt install aspell aspell-en
(use-package markdown-mode
  :ensure t
  ;;  :defer t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :hook
  (markdown-mode . auto-fill-mode)
  :config
  (when (executable-find "aspell")
    (add-hook 'markdown-mode-hook #'flyspell-mode))
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown")))

;;; https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
(use-package kotlin-mode
  :ensure t)

;;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

;;;; LaTeX/AucTeX
;;;; !NOTE: to automatically compile and update PDF preview use:
;;;; https://www.reddit.com/r/emacs/comments/k7sx2n/latexpreviewpane_and_latexmk/
(use-package tex
  :ensure auctex
  :custom
  (font-latex-script-display nil)
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color)
  (TeX-auto-save t)
  (Tex-parse-self t)
  (TeX-PDF-mode t) ; PDF mode by default
  :hook
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)  ; easy math input
  (LaTeX-mode . turn-on-reftex)  ; RefTeX integration
  (LaTeX-mode . (lambda () (setq show-trailing-whitespace t))))

;;; https://github.com/minad/vertico
(use-package vertico
  :ensure t)

;;; https://github.com/minad/corfu
(use-package corfu
  :ensure t)

;; TBD to watch about `orderless` package by @prot
;; https://youtu.be/d3aaxOqwHhI?t=1929

;; @prot sample configuration including `orderless` package
;; https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/

;;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; https://github.com/protesilaos/denote
(use-package denote
  :ensure t)

;;; https://github.com/minad/marginalia
;;(use-package marginalia
;;  :ensure t
;;  :init
;;  (marginalia-mode))

(use-package modus-themes
  :ensure t)
; :demand t
; :init
; (modus-themes-include-derivatives-mode 1))
  
;; TREE-SITTER (ts)
;;; GitHub: https://github.com/tree-sitter/tree-sitter

;;; TBD to read about how to get started w/ Tree-Sitter
;;; URL: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;;; json-mode.el
;;;; https://elpa.gnu.org/packages/json-mode.html
(use-package json-mode
  :ensure t)

;;; prism.el
;;;; https://github.com/alphapapa/prism.el
(use-package prism
  :ensure t
  :defer t
  :hook
  ;; activate prism for C-based major modes
  ((json-mode) . prism-mode)
  ((python-mode python-ts-mode haskell-mode) . prism-whitespace-mode))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
