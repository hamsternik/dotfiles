;;; early-init.el --- Loaded before packages and the GUI frame -*- lexical-binding: t -*-

;; Native compilation on newer macOS.
;; The libgccjit bundled with Emacs.app derives `-mmacosx-version-min' from
;; the Darwin kernel version and gets it wrong on newer macOS (Darwin 27 ->
;; 18.0), so clang rejects every native compile ("invalid version number").
;; Pass the real macOS major version explicitly. "-Wl,-w" is the default
;; value, kept because setting the variable here replaces it.
(when (and (eq system-type 'darwin)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
    (let ((macos-major
           (car (split-string
                 (shell-command-to-string "sw_vers -productVersion")
                 "[.\n]" t))))
        (when macos-major
            (setq native-comp-driver-options
                  (list "-Wl,-w"
                        (concat "-mmacosx-version-min=" macos-major))))))

;; Don't pop up *Warnings* for packages' own compiler warnings (e.g. optional
;; integrations they call but don't depend on). They are still logged in
;; the *Async-native-compile-log* buffer.
(setq native-comp-async-report-warnings-errors 'silent)
