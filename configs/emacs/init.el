;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; - C-h f custom-file to see the function decl;
;;; - C-h v custom-file to see the variable decl and example of custom-file usage.
;; Set custom-file path based on platform
;; macOS/Darwin: ~/.config/emacs/custom.init.el
;; Linux/WSL: ~/.emacs.d/custom.init.el
(setq custom-file
      (if (eq system-type 'darwin)
          "~/.config/emacs/custom.init.el"
        "~/.emacs.d/custom.init.el"))
(load custom-file) ;; OR (load-file custom-file)

(load-theme 'gruber-darker' t) ;; to load `gruber-darker` custom theme in emacs 24+

;;; FIXME: Fira Code font does not work properly in Standalone Emacs
;;; TBD to check out workaround here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
(set-face-attribute 'default nil
		    :font "Fira Code-14")
(set-fontset-font t 'latin "Iosevka-14" nil 'append)
(set-fontset-font t 'latin "Roboto Mono-14" nil 'append)


;; scroll Emacs like lightning (macOS)
;;; https://github.com/jdtsmith/ultra-scroll
;;; based on https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; (global-display-line-numbers-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t) ;; to disable emacs splash screen
;; (setq initial-scratch-message nil) ;; to remove initial message in *scratch*

;;; Dired:
;;; the Directory Editor
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html 

(setq dired-use-ls-dired nil)
;; (setq initial-buffer-choice t)
(setq initial-buffer-choice (lambda () (dired "~")))

(defun open-dired-at-current-file ()
  "Open `dired` in the directory of the currenly opened file."
  (interactive)
  (if buffer-file-name
      (dired (file-name-directory buffer-file-name))
    (message "No file associated with this buffer.")))

(global-set-key (kbd "C-c d") 'open-dired-at-current-file)

;; restore the last emacs session, including the buffer for the file, *scratch* buffer, etc
(desktop-save-mode 1)
;; enables `auto-revert-mode` globally, makes emacs automatically reload files if they are modified outside of emacs
(global-auto-revert-mode 1)
;; [!NOTE]: This code does not work for the *scratch* buffer specifically. Need to store *scratch* buffer content manually to a file.
;;(setq desktop-buffers-not-to-save (delete "\\*scratch\\*" desktop-buffers-not-to-save))

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


;;; emacs IDO MODE
;;; ==============

(ido-mode 1) ;; enable ido-mode for better switching
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

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

;; Today I installed all packages in the `M-x package-list-packages`.
;; All packages are installed in the `custom.init.el` file.

;; use-package
;;; init & config emacs features

(use-package markdown-mode
  :ensure t
  ;; :defer t
  ;; :config
  ;; (setq markdown-fontify-code-blocks-natively t)
  )

;; TBD to watch about `orderless` package by @prot
;; https://youtu.be/d3aaxOqwHhI?t=1929

;; @prot sample configuration including `orderless` package
;; https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/

;;; eglot
;;; a client for LSP servers
;;; https://github.com/joaotavora/eglot

(use-package swift-mode
  :mode "\\.swift\\'")

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

(use-package eglot
  ;; :defer t
  ;; :hook ((python-mode . eglot-ensure))
  ;; :custom
  ;; (eglot-report-progress nil)  ; Prevent minibuffer spam
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '((swift-mode) . hamsternik/sourcekit-lsp-command)))
