# CLAUDE.md

> Project context for Claude Code AI assistant.

## Overview

Personal dotfiles repository for macOS development environment. Manages 25+ tools and applications through symlink-based installation automated via Makefile.

**Primary focus:** macOS (no Linux support planned)

## Project Structure

```
dotfiles/
├── bin/                          # Custom scripts and automation
│   ├── macos/                    # macOS-specific (brew-install, networkup.sh)
│   ├── ruby/                     # Ruby/rbenv utilities
│   ├── git/                      # Git helpers (git-grep, git-show-submodules)
│   ├── vscode/                   # VS Code extension management
│   ├── tmux/                     # Tmux utilities
│   └── antibody/                 # Zsh plugin manager setup
│
├── configs/                      # All configuration files
│   ├── emacs/                    # Emacs (init.el, custom.init.el)
│   ├── nvim/                     # NeoVim (Lua-based config)
│   ├── vim/                      # Vim (vimrc)
│   ├── vscode/                   # VS Code settings, keybindings, extensions
│   ├── zed/                      # Zed editor
│   ├── sublime-text-4/           # Sublime Text 4
│   │
│   ├── fish/                     # Fish shell (primary shell)
│   ├── zsh/                      # Zsh configuration
│   ├── bash/                     # Bash configuration
│   │
│   ├── git/                      # Git config (personal + work)
│   ├── ssh/                      # SSH host configuration
│   ├── gnupg/                    # GPG signing configuration
│   │
│   ├── kitty/                    # Kitty terminal
│   ├── ghostty/                  # Ghostty terminal
│   ├── tmux/                     # Tmux multiplexer
│   ├── zellij/                   # Zellij (tmux alternative)
│   │
│   ├── karabiner/                # Keyboard customization
│   ├── finicky/                  # Browser routing
│   ├── lf/                       # Terminal file manager
│   ├── xcode/                    # Xcode key bindings (custom.idekeybindings)
│   ├── claude-desktop/           # Claude Desktop config
│   └── crush/                    # Crush AI config
│
├── Makefile                      # Installation automation (symlinks)
├── Brewfile                      # Homebrew packages (100+ packages)
└── README.md                     # Setup documentation
```

## Xcode Configuration

**Location:** `configs/xcode/custom.idekeybindings`

Custom Xcode key bindings in XML plist format (`.idekeybindings`). Xcode loads all `.idekeybindings` files found in `~/Library/Developer/Xcode/UserData/KeyBindings/` and exposes each as a selectable profile under **Settings → Key Bindings**.

```bash
make install-xcode-keybinds    # symlink custom.idekeybindings into Xcode's KeyBindings dir
make uninstall-xcode-keybinds  # remove the symlink
```

Modifier notation used in the file: `@` = Cmd, `$` = Shift, `^` = Ctrl, `~` = Option.

## Key Commands

```bash
make install-all              # Install all configurations
make uninstall-all            # Remove all symlinks
make install-<tool>-conf      # Install specific tool (e.g., install-emacs-conf)
make install-brew             # Install Homebrew packages from Brewfile
make emacs-lint               # Validate Emacs config (batch load, reports errors)
```

## Emacs Configuration

**Location:** `configs/emacs/init.el`

### Package Management

- Uses `package.el` with MELPA and NonGNU archives
- `use-package` for declarative configuration

### Included Packages

| Category | Packages |
|----------|----------|
| Completion | corfu, cape, orderless, marginalia |
| UI | modus-themes, telephone-line |
| Terminal | multi-vterm |
| Languages | swift-mode, kotlin-mode, json-mode, markdown-mode, cmake-mode, auctex |
| LSP | eglot (built-in), tree-sitter |
| Utilities | exec-path-from-shell, prism |

### Built-in Packages Configured

- `dired` - Directory editor
- `eldoc` - Inline documentation
- `flymake` - Syntax checking
- `flyspell` - Spell checking (aspell)
- `electric-pair` - Auto-close delimiters
- `org-mode` - Note taking and task management
- `project` - Project management
- `doc-view` - PDF viewing

### Key Bindings (macOS)

| Binding | Action |
|---------|--------|
| `s-Z` | Undo/redo |
| `s-O` | Project find file |
| `s-S` | Switch buffer |
| `s-w` | Close window |
| `s-/` | Comment line |
| `s-\` | Split right |
| `s-\|` | Split below |
| `s-{` / `s-}` | Navigate windows |
| `C-c C-r` | Reload config |
| `C-x C-i` | Indent buffer |

### TBD / Commented Code

1. **Vertico** (lines 431-435) - Vertical completion UI, currently disabled:
   ```elisp
   ;; (use-package vertico
   ;;   :ensure t
   ;;   :hook (after-init . vertico-mode))
   ```

2. **Ultra Scroll** (lines 518-522) - Smooth scrolling for macOS, mentioned but not configured

3. **Eglot Python hook** (line 617) - LSP for Python commented out:
   ```elisp
   ;; :hook ((python-mode . eglot-ensure))
   ```

4. **Tree-sitter** (lines 637-644) - Noted as "too complicated" for WSL, partial config exists

5. **Fira Code ligatures** (lines 223-228) - Font ligatures workaround needed

### Corfu + Cape + Makefile Mode

**Problem:** Corfu completion doesn't work in Makefile buffers.

**Root cause:** Makefile modes (gmake, bsdmake, automake) set buffer-local `completion-at-point-functions` that override global cape capfs:

```
Global:       (cape-dabbrev cape-file ...)        <- your config
Buffer-local: (makefile-completions-at-point t)  <- makefile-mode overrides
```

**Solution:** Add cape capfs directly to makefile buffers via hook in cape config:

```elisp
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (add-hook 'makefile-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-dabbrev))))
```

**Debug commands:**
```
M-x describe-variable RET completion-at-point-functions  ;; check local vs global
M-x cape-dabbrev                                         ;; test cape directly
M-/                                                      ;; test dabbrev-expand
```

## Emacs Validation Tools

**Built-in tools for validating elisp config:**

| Tool | Purpose | Usage |
|------|---------|-------|
| `byte-compile-file` | Compiles elisp, reports warnings/errors | `M-x byte-compile-file RET init.el` |
| `check-parens` | Validates balanced parentheses | `M-x check-parens` |
| `elint-current-buffer` | Lints elisp code | `M-x elint-current-buffer` |
| `checkdoc` | Validates docstrings | `M-x checkdoc` |

**Batch validation from terminal:**
```bash
make emacs-lint               # preferred: runs emacs --batch -l configs/emacs/init.el
```

**Quick validation workflow:**
1. `M-x check-parens` - catch syntax errors
2. `M-x byte-compile-file` - catch semantic issues (check `*Compile-Log*` buffer)
3. `make emacs-lint` - batch load test from terminal
4. `C-c C-r` - test actual loading inside Emacs

## Makefile Patterns

### Variable naming conventions

| Pattern | When to use | Example |
|---------|-------------|---------|
| `TOOL_SOURCE` + `TOOL_DEST` | File-to-file symlinks | `FINICKY_SOURCE`, `GIT_SOURCE` |
| `TOOL_DIR` | Directory-based configs, multiple files symlinked from one dir | `FISH_DIR`, `NVIM_DIR` |

### Target structure

Every tool follows this pair pattern:

```makefile
install-X-conf:
	@$(MAKE) uninstall-X-conf        # always clean before installing
	@echo "\n✨ Installing X config files."
	ln -s -n $(X_DIR)/file ~/.config/x/file

uninstall-X-conf:
	rm ~/.config/x/file || true      # suppress error if symlink missing
```

### Symlink flags

- `ln -s -f` — for **individual files** (overwrites existing target)
- `ln -s -n` — for **directories** (treats destination as normal file, not a dir inside dir)
- Never use `ln -s` alone — always specify `-f` or `-n`

### Pre-condition guards

Check that a required directory exists before symlinking into it:

```makefile
@if [ ! -d "$$HOME/.config/nvim" ]; then echo "dir does not exist. Exit."; exit 1; fi
```

Note: use `$$HOME` (escaped) inside recipe shell commands, `$(HOME)` in Make variable context.

### Echo / messaging convention

- Prefix with `\n` for visual separation
- Emoji goes at the **end** of the sentence, not the beginning
- Always use `🚀` for all install echo messages
- Use `@echo` (silent) for announcements, bare `echo` only when the output itself is the content

### install-all / uninstall-all

Both batch targets list every tool explicitly via `$(MAKE) install-X-conf` calls. When adding a new tool, add it to **both** batch targets.

### Known inconsistencies (do not replicate)

- `uninstall-all`: `uninstall-gpg-conf` is commented out, but `install-all` includes it

## Code Style

- Emacs: 4-space indentation, no tabs
- General: Follow existing patterns in each config file
