;;  -*- coding: utf-8; lexical-binding: t -*- 

;;; Intro:
;;; See https://github.com/patrickt/emacs for inspiration.
;;; See https://github.com/LionyxML/emacs-kick to build upon kickstarter config.

;;; Emacs Wiki:
;;; EmascForMacOS, https://www.emacswiki.org/emacs/EmacsForMacOS

;;; Magit: Essential Settings
;;; See https://docs.magit.vc/magit/Essential-Settings.html

;;; Commentary:
;;; - C-h f custom-file to see the function decl;
;;; - C-h v custom-file to see the variable decl and example of custom-file usage.

;; Package initialization first. In Emacs, a package is a collection of
;; Elisp code that extends the editor's functionality. Import this package
;; to add package archives. Then, add MELPA and nonGNU to the list of
;; package archives. By default, Emacs comes with ELPA configured.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(defun delete-to-start-of-line ()
  "Delete all content from the cursor to the start of the line."
  (interactive)
  (delete-region (line-beginning-position) (point)))
(bind-key "s-<backspace>" 'delete-to-start-of-line)

;; reload emacs config
(defun reload-emacs-config ()
  "Reload emacs.el Emacs configuration file"
  (interactive)
  (load-file user-init-file))

;; create new empty *untitled* buffer
(defun create-empty-buffer () 
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)))

;; re-indent the entire emacs buffer
(defun indent-buffer ()
  "Indent an entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; close the 2nd (split) window in the buffer
(defun close-current-window ()
  "Close the currently active split window."
  (interactive)
  (if (not (one-window-p))
      (delete-window)
    (message "Cannot close the only window")))

;; try to move right, fallback to down
(defun hamsternik/windmove-right-or-down ()
  "Switch focus on right or bottom window in buffer."
  (interactive)
  (unless (ignore-errors (windmove-right))
    (ignore-errors (windmove-down))))

(defun hamsternik/windmove-left-or-up ()
  "Switch focus on left or top window in buffer."
  (interactive)
  (unless (ignore-errors (windmove-left))
    (ignore-errors (windmove-up))))

(defvar hn/scratch-file
  (expand-file-name "scratch.txt" user-emacs-directory))
(defun hn/save-scratch ()
  "Save *scratch* buffer data in the local ~/.emacs.d file."
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max) hn/scratch-file)))
(defun hn/restore-scratch ()
  "Restore *scratch* buffer from the local ~/.emacs.d file."
  (when (file-exists-p hn/scratch-file)
    (with-current-buffer "*scratch*"
      (delete-region (point-min) (point-max))
      (insert-file-contents hn/scratch-file))))

(defun hn/copy-line ()
  "Copy the current line including newline to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2))
  (message "Line copied"))

;; EMACS CONFIG
(use-package emacs
  :ensure nil
  :custom
  ;; disable automatic saving on buffers. 
  ;;(auto-save-default nil)
  ;; restore the last emacs session, including *scratch* buffer latest changes
  (desktop-save-mode 1)
  (display-line-numbers-type 'absolute)
  (delete-selection-mode 1)
  ;; enables `auto-revert-mode` globally, makes emacs automatically reload files if they are modified outside of emacs
  (global-auto-revert-mode 1)
  ;; [!NOTE]: This code does not work for the *scratch* buffer specifically. Need to store *scratch* buffer content manually to a file.
  ;;(setq desktop-buffers-not-to-save (delete "\\*scratch\\*" desktop-buffers-not-to-save))
  (inhibit-startup-screen t)
  ;; disable the use of tabs for indentation (spaces instead)
  (indent-tabs-mode nil)
  ;; enable indentation+completion using the TAB key
  (tab-always-indent 'complete)
  ;; disable Ispell completion function in Emacs 30+ and try 'cape-dict as an alternative
  (text-mode-ispell-word-completion nil)
  (tab-width 4)
  (truncate-lines t)
  (use-short-answers t)
  (visible-bell t)

  ;; M-! alternative to C-;
  :bind (("C-;" . shell-command)
         ;;; C-x KEYBINDS
         ("C-x r" . undo-redo)
         ("C-x C-/" . 'comment-or-uncomment-region)
         ;; Fix the indentation of an entire buffer.
	     ;; To indent a region, highlight the text and use C-i.
         ("C-x C-i" . 'indent-buffer)
         ;;; C-c KEYBINDS
         ("C-c C-r" . 'reload-emacs-config)         
         ("C-c o" . buffer-menu)
         ;; re-bind `eval-last-sexp` with `eval-buffer` to evaluate an entire buffer atm
         ;;("C-x C-e" . 'eval-buffer)
         ;; bind `eval-last-sexp` to a non-standard key bind
         ;;("C-c C-e" . 'eval-last-sexp)
         ("C-c n" . 'create-empty-buffer)
         ("C-c r" . 'rename-buffer)
         ("M-p" . yank)
         ("M-P" . yank-pop)
         ;; Bind =Yank from kill-ring=
         ("M-W" . 'hn/copy-line))
  
  :hook
  (fundamental-mode . outline-minor-mode)
  (text-mode . outline-minor-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . hs-minor-mode)

  :config
  ;; Set custom filepath to keep all nongnu/melpa plugins.
  ;; macOS/Darwin: ~/.config/emacs/custom.init.el
  ;; Linux/WSL: ~/.emacs.d/custom.init.el
  (setq custom-file
        (if (eq system-type 'darwin)
            "~/.config/emacs/custom.init.el"
          "~/.emacs.d/custom.init.el"))
  (load custom-file)

  ;; disable all built-in panels in Emacs in GUI mode
  (when (window-system)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (menu-bar-mode 0))
  
  ;; macOS Command is a built-in Super key `s`
  (when (eq system-type 'darwin)
    (bind-key "s-Z" 'undo-redo)
    ;; to find a project-specific file, Xcode-related
    (bind-key "s-O" #'project-find-file)
    ;; to switch between emacs buffer
    (bind-key "s-S" #'switch-to-buffer)
    (bind-key "s-w" 'close-current-window)
    (bind-key "s-/" 'comment-line)
    (bind-key "s-\\" #'split-window-right)
    (bind-key "s-|" #'split-window-below)
    (bind-key "s-{" 'hamsternik/windmove-left-or-up)
    (bind-key "s-}" 'hamsternik/windmove-right-or-down))

  ;; macOS re-map all control keys
  (when (eq system-type 'darwin)
    ;; make Commnad key act as Meta
    (setq mac-command-modifier 'meta)
    ;; keep Option/Alt as Meta
    (setq mac-option-modifier 'meta)
    ;; make Control stay as Control
    (setq mac-control-modifier 'control)
    ;; optional: make right Command act as Super instead
    (setq mac-right-command-modifier 'super))

  (add-hook 'kill-emacs-hook 'hn/save-scratch)
  (add-hook 'after-init-hook 'hn/restore-scratch))

;; EMACS POST-CONFIG

;; keeping auto-save files enabled but moving files to a central dir:
(make-directory (expand-file-name "auto-save/" user-emacs-directory) t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; EMACS FACE FONT
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

;;; FIXME: Fira Code font does not work properly in Standalone Emacs
;;; TBD to check out workaround here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;(set-face-attribute 'default nil
;;		    :font "Fira Code-14")
;;(set-fontset-font t 'latin "Iosevka-14" nil 'append)
;;(set-fontset-font t 'latin "Roboto Mono-14" nil 'append)

;;; EMACS THEME
;; (load-theme 'gruber-darker' t)

;;; MODUS THEMES
;; https://github.com/protesilaos/modus-themes
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

;; ORG-MODE
;; Org mode is a powerful system for organizing and managing your notes,
;; tasks, and documents in plain text. It offers features like task
;; management, outlining, scheduling, and much mroe, making it a versatile
;; tool for productivity. The configuration below simply deferes loading
;; Org-mode until it is explicitly needed, which can help speed up Emacs
;; startup time.
(use-package org
  :ensure nil
  :defer t

  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(p)" "CANCELED(c)" "|" "DONE(d)")))

  ;; Format: (INACTIVE-FORMAT . ACTIVE FORMAT)
  ;; %a abbreviated day name
  ;; %b abbreviated month name
  ;; %d day of month
  ;; %Y year
  (org-time-stamp-custom-formats '("<%a, %b %d %Y>" . "<%a, %b %d %Y>"))
  ;; Optional: to turn on custom format by-default.
  (org-display-custom-times t)
  (browse-url-browser-function 'browse-url-default-browser))

;;; DIRED
;; the directory editor
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
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

;;; PROJECT
(use-package project
  :ensure nil)

;;; ANSI COLOR
;; enable ANSI color support in compilation buffers
;; based on https://github.com/anschwa/emacs.d?tab=readme-ov-file#ansi-color-codes
;; FIXME: broken ansi symbols in standard fish greeting message when run `C-x p s` shell
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

;;; ELDOC
;; Eldoc provides helpful inline documentation for functions and variables
;; in the minibuffer, enhancing the development experience. It can be
;; particulary useful in programming modes, as it helps you to understand
;; the context of functions as you type. The following line enables Eldoc
;; globally for all files.
(use-package eldoc
  :ensure nil
  :config
  ;; automatically fetch doc help
  (setq eldoc-idle-delay 0.1)
  ;; use the "K" floating help instead
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; FIXME: set to `t` if you want docs on the echo area
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

;;; FLYSPELL
(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "aspell"))

;; FLYMAKE
;; Flymake is an on-the-fly syntax checking extension that provides
;; real-time feedback about erros and warnings in your code as you write.
;; The configuration below activates Flymake mode in programming buffers.
(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error)
     (warning "»" compilation-warning)
     (note "»" compilation-info))))

;; DOC-VIEW
(use-package doc-view
  :ensure nil
  :mode ("\\.pdf\\'" . doc-view-mode)
  :custom
  (doc-view-resolution 300)
  (doc-view-continuous t))

;;; NONGNU PACKAGES
;;; MELPA PACKAGES

;;; MODE LINE
;; https://github.com/dbordak/telephone-line
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

;;; VERTICO
;; https://github.com/minad/vertico
;; Vertico enhances the completion experience in Emacs by providing a
;; vertical selection interface for both buffer and minibuffers completions.
;; Unlike traditional minibuffer completion, which displays candidates
;; in a horizontal format, Vertico presents candidates in a vertical list,
;; macking it easier to browse and select from multiple options.
;; (use-package vertico
;;   :ensure t
;;   :hook
;;   ;; enable vertico after Emacs has initialized
;;   (after-init . vertico-mode))

;;; CAPE
;; https://github.com/minad/cape
;; Completion at point extension. Cape provides Completion At Point Extensions which
;; can be used in combination with Corfu, Company or the default completion UI,
;; e.g. 'dabbrev as dynamic abbreviation. The completion backends used by
;; `completion-at-point` are so called `completion-at-point-functions` (Capfs).
;; BTW no need to configure the 'dabbrev built-in package for auto-complete feature,
;; as cape-dabbrev is a thin wrapper around the build-in 'dabbrev functionality.
;; It uses 'dabbrev under the hood with its default settings, which work well for most cases.
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; CORFU
;; https://github.com/minad/corfu
;; Corfu mode provides a text completion framework for Emacs. It enhances
;; the editing experience by offering context-aware suggestions as you
;; type. Highly customizable and can be integrated with various modes and languages.
(use-package corfu
  :ensure t
  :defer t
  :custom
  ;; completes when hitting TAB /only/ 
  (corfu-auto t)
  ;; delay before popup; enable if corfu-auto is t
  (corfu-auto-delay 0.2)
  ;; trigger completion after typing 2 characters
  (corfu-auto-prefix 2)
  ;; quit popup if no match
  (corfu-quit-no-match t)
  ;; margin when scrolling completions
  (corfu-scroll-margin 5)
  ;; delay before showing documentation popup
  (corfu-popupinfo-delay 0.5)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  
  :init
  (corfu-popupinfo-mode t))

;;; MARGINALIA
;; https://github.com/minad/marginalia
;; The `marginalia package provides helpful annotations next to
;; completion candidates in the minibuffer. The information on display
;; depends on the type of content. If it is about files, it shows
;; file permissions and the last modified date. If it is a buffer,
;; it shows the buffer size, major mode and other things.
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; marginalia must be activated in the :init section of use-package
  ;; such that the mode gets enabled right away. Note that this
  ;; forces loading the package.
  (marginalia-mode)

  :config
  (fido-vertical-mode 1))

;;; Orderless
;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; TODO: @prot sample configuration including `orderless` package
;; https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages


;;; ULTRA SCROLL
;; https://github.com/jdtsmith/ultra-scroll
;; Scroll Emacs like lightning (macOS).
;; Based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

;;; DENOTE
;; https://github.com/protesilaos/denote
(use-package denote
  :ensure t)

;;; TEX and LATEX
;; !NOTE: to automatically compile and update PDF preview use:
;; https://www.reddit.com/r/emacs/comments/k7sx2n/latexpreviewpane_and_latexmk/
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

;;; MARKDOWN
;; https://github.com/jrblevin/markdown-mode
;; TBD to verify whether `aspell` is installed
;; macOS: brew install aspell
;; Win/WSL: sudo apt install aspell aspell-en
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

;;; KOTLIN MODE
;; https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
(use-package kotlin-mode
  :ensure t)

;;; SWIFT MODE
;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

;;; JSON MODe
;; https://elpa.gnu.org/packages/json-mode.html
(use-package json-mode
  :ensure t)

;;; PRISM
;; https://github.com/alphapapa/prism.el
(use-package prism
  :ensure t
  :defer t
  :hook
  ;; activate prism for C-based major modes
  ((json-mode) . prism-mode)
  ((python-mode python-ts-mode haskell-mode) . prism-whitespace-mode))

;;; CMAKE MODE
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

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

;;; EGLOT
;; https://github.com/joaotavora/eglot
;; A client for LSP servers; built-in since Emacs 29.
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

;; TREE-SITTER (ts)
;;; GitHub: https://github.com/tree-sitter/tree-sitter

;;; TBD to read about how to get started w/ Tree-Sitter
;;; URL: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(provide 'init)
;;; init.el ends here
