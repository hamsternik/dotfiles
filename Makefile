# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
# https://xs-labs.com/en/blog/2020/11/07/introduction-to-makefiles/

# Warning:
# Do not use`rm -rf path/to/dir/` for symlinks applied to directories.
# To remove the dir symlink, only use `rm path/to/dir_symlink` w/o last dash.
# Then you will remove only the symlink on the disk.

# Warning:
# To symlink the dir in macOS (the right way), you need to use `-n` .
# -n flag treats the destination as file, even if it is a dir.

# Warning:
# To suppress errors and skip the command when it fails (returns non-zero exit code):
# Use `-` before a command to ignore errors and continue with next one.
# Use `|| true` in the end of the command to suppress the error.

CURRENTDIR := $(shell pwd)

help:
	@cat Makefile

install-brew:
	brew bundle install --file=Brewfile

## mas, Mac App Store command-line interface, https://github.com/mas-cli/mas
install-mas:
# @echo "Agenda, note taking application focusing on dates"
# mas install 1287445660
	@echo "Bear, beautiful, powerfully simple Markdown app to capture, write, and organize your life."
	mas install 1091189122
# @echo "Flow, a pomodoro based work timer to help you get things done."
# mas install 1423210932
	@echo "PDF Viewer Pro by PSPDFKit"
	mas install 1120099014

# Install config files, macOS oriented.

install-configs:
	$(MAKE) install-finicky
	$(MAKE) install-fish
	$(MAKE) install-git
	$(MAKE) install-gpg
	$(MAKE) install-karabiner
	$(MAKE) install-kitty
	$(MAKE) install-lf
	$(MAKE) install-nvim
	$(MAKE) install-shell
	$(MAKE) install-tmux
	$(MAKE) install-vim
	$(MAKE) install-vscode
	$(MAKE) install-zellij
	$(MAKE) install-zsh

uninstall-configs:
	$(MAKE) uninstall-finicky
	$(MAKE) uninstall-fish
	$(MAKE) uninstall-git
# $(MAKE) uninstall-gpg
	$(MAKE) uninstall-karabiner
	$(MAKE) uninstall-kitty
	$(MAKE) uninstall-lf
	$(MAKE) uninstall-nvim
	$(MAKE) uninstall-shell
	$(MAKE) uninstall-tmux
	$(MAKE) uninstall-vim
	$(MAKE) uninstall-vscode
	$(MAKE) uninstall-zellij
	$(MAKE) uninstall-zsh

## finicky configs (Utility for customizing which browser to start)

FINICKY_DIR := $(CURRENTDIR)/configs/finicky
install-finicky:
	@echo "\n✨ Installing finicky.js config files..."
	ln -s -n $(FINICKY_DIR)/finicky.js ~/.finicky.js
.PHONY: install-finicky

uninstall-finicky:
	rm ~/.finicky.js || true
.PHONY: uninstall-finicky

## fish shell

FISH_DIR := $(CURRENTDIR)/configs/fish
install-fish:
	@echo "\n✨ Installing fish shell config files..."
	if [ ! -d "$$HOME/.config/fish" ]; then mkdir -p "$$HOME/.config/fish"; else echo "⚠️ ~/.config/fish already exists."; fi
	ln -s $(FISH_DIR)/config.fish ~/.config/fish/config.fish
# ln -s $(FISH_DIR)/fish_plugins ~/.config/fish/fish_plugins
# @echo "✨ Installed fish config files. Installing config directories..."
# ln -s -n $(FISH_DIR)/completions ~/.config/fish/completions
# ln -s -n $(FISH_DIR)/conf.d ~/.config/fish/conf.d
# ln -s -n $(FISH_DIR)/functions ~/.config/fish/functions
# ln -s -n $(FISH_DIR)/themes ~/.config/fish/themes
.PHONY: install-fish

uninstall-fish:
	rm ~/.config/fish/config.fish || true
# rm ~/.config/fish/fish_plugins || true
# rm ~/.config/fish/completions || true
# rm ~/.config/fish/conf.d || true
# rm ~/.config/fish/functions || true
# rm ~/.config/fish/themes || true
.PHONY: uninstall-fish

## git configs

GIT_DIR := $(CURRENTDIR)/configs/git
install-git:
	@echo "\n✨ Installing git config files..."
	ln -s -n $(GIT_DIR)/gitattributes ~/.gitattributes
	ln -s -n $(GIT_DIR)/gitignore_global ~/.gitignore_global
	ln -s -n $(GIT_DIR)/gitconfig ~/.gitconfig
.PHONY: install-git

uninstall-git:
	rm ~/.gitattributes || true
	rm ~/.gitignore_global || true
	rm ~/.gitconfig || true
.PHONY: uninstall-git

## gnupg configs

GNUPG_DIR := $(CURRENTDIR)/configs/gnupg
install-gpg:
	@echo "GPG public key(ring) is not used at the moment."
# brew list gnupg || HOMEBREW_NO_AUTO_UPDATE=1 brew install gnupg
# ln -s -n $(GNUPG_DIR)/gpg-agent.conf ~/.gnupg/gpg-agent.conf
# ln -s -n $(GNUPG_DIR)/gpg.conf ~/.gnupg/gpg.conf
.PHONY: install-gpg

## karabiner configs (Keyboard customiser)

KARABINER_DIR := $(CURRENTDIR)/configs/karabiner
install-karabiner:
	@echo "\n✨ Installing Karabiner config files..."
	ln -s -n $(KARABINER_DIR)/karabiner.json ~/.config/karabiner/karabiner.json
	ln -s -n $(KARABINER_DIR)/change-lang-one-key.json ~/.config/karabiner/assets/complex_modifications/change-lang-one-key.json
.PHONY: install-karabiner

uninstall-karabiner:
	rm ~/.config/karabiner/karabiner.json || true
	rm ~/.config/karabiner/assets/complex_modifications/change-lang-one-key.json || true
.PHONY: uninstall-karabiner

## kitty configs (GPU-based terminal emulator on Python)

KITTY_DIR := $(CURRENTDIR)/configs/kitty
install-kitty:
	@echo "\n✨ Installing kitty terminal config files..."
	ln -s -n $(KITTY_DIR)/kitty.conf ~/.config/kitty/kitty.conf
.PHONY: install-kitty

uninstall-kitty:
	rm ~/.config/kitty/kitty.conf || true
.PHONY: uninstall-kitty

## lf configs (Terminal file manager)

LF_DIR := $(CURRENTDIR)/configs/lf
install-lf:
	@echo "\n✨ Installing lf config files..."
	ln -s -n $(LF_DIR)/lfrc ~/.config/lf/lfrc
.PHONY: install-lf

uninstall-lf:
	rm ~/.config/lf/lfrc || true
.PHONY: uninstall-lf

## nvim configs

NVIM_DIR := $(CURRENTDIR)/configs/nvim
install-nvim:
	@echo "\n✨ Installing NeoVim config files..."
	if [ ! -d "$$HOME/.config/nvim" ]; then mkdir -p "$$HOME/.config/nvim"; else echo "~/.config/nvim already exists."; fi
	ln -s -n $(NVIM_DIR)/init.lua ~/.config/nvim/init.lua
	ln -s -n $(NVIM_DIR)/lua ~/.config/nvim/lua
	ln -s -n $(NVIM_DIR)/plugin ~/.config/nvim/plugin
.PHONY: install-nvim

uninstall-nvim:
	rm ~/.config/nvim/init.lua || true
	rm ~/.config/nvim/lua || true
	rm ~/.config/nvim/plugin || true
.PHONY: uninstall-nvim

## basic shell configs

SHELL_DIR := $(CURRENTDIR)/configs/shell
install-shell:
	@echo "\n✨ Installing bash & shell config files..."
	ln -s -n $(SHELL_DIR)/aliases ~/.aliases
	ln -s -n $(SHELL_DIR)/bashrc ~/.bashrc
	ln -s -n $(SHELL_DIR)/profile ~/.profile
.PHONY: install-shell

uninstall-shell:
	rm ~/.aliases || true
	rm ~/.bashrc || true
	rm ~/.profile || true
.PHONY: uninstall-shell

# TMUX config (Terminal multiplexer)

TMUX_DIR := $(CURRENTDIR)/configs/tmux
install-tmux:
	@echo "\n✨ Installing TMUX config files..."
	ln -s -n $(TMUX_DIR)/tmux.conf ~/.tmux.conf
.PHONY: install-tmux

uninstall-tmux:
	rm ~/.tmux.conf || true
.PHONY: uninstall-tmux

## Terminal.app (default macOS terminal app)

TERMINAL_DIR := $(CURRENTDIR)/configs/terminal-macos
install-terminal:
	@echo "Use Atom.terminal theme in the Termainl.app -> Settings -> Profiles -> More -> Import..."
.PHONY: install-terminal

## ViM configs (Vi 'workalike' with many additional features)

VIM_DIR := $(CURRENTDIR)/configs/vim
install-vim:
	@echo "\n✨ Installing ViM config files..."
	if [ ! -d "$$HOME/.vim" ]; then mkdir -p "$$HOME/.vim"; else echo "~/.vim already exists."; fi
	ln -s -n $(VIM_DIR)/vimrc ~/.vim/.vimrc
.PHONY: install-vim

uninstall-vim:
	rm ~/.vim/.vimrc || true
.PHONY: uninstall-vim

## VSCode configs (Open-source code editor)

VSCODE_DIR := $(CURRENTDIR)/configs/vscode
install-vscode:
	@echo "\n✨ Installing VSCode config files..."
	ln -s -n $(VSCODE_DIR)/keybindings.json ~/Library/Application\ Support/Code/User/keybindings.json
	ln -s -n $(VSCODE_DIR)/settings.json ~/Library/Application\ Support/Code/User/settings.json
.PHONY: install-vscode

uninstall-vscode:
	rm ~/Library/Application\ Support/Code/User/keybindings.json || true
	rm ~/Library/Application\ Support/Code/User/settings.json || true
.PHONY: uninstall-vscode

## zellij configs (Pluggable terminal workspace, with terminal multiplexer as the base feature)

ZELLIJ_DIR := $(CURRENTDIR)/configs/zellij
install-zellij:
	@echo "\n✨ Installing zellij config files..."
	ln -s -n $(ZELLIJ_DIR)/config.kdl ~/.config/zellij/config.kdl
.PHONY: install-zellij

uninstall-zellij:
	rm ~/.config/zellij/config.kdl || true
.PHONY: uninstall-zellij

doc-zellij:
	@echo "There are 2 config files available, config.kdl and default.config.kdl in the repo."
	@echo "The config.default.kdl is generated by zellij if there is no config files in the ~/.config/zellij dir by-default."
.PHONY: doc-zellij

## Zsh configs (shell designed for interactive use, although it is also a powerful scripting language)

ZSH_DIR := $(CURRENTDIR)/configs/zsh
install-zsh:
	@echo "\n✨ Installing Zsh config files..."
	@echo "TBD find zsh-git-prompt plugin or theme with build-in git using antigen PM:"
	@echo "antigen: https://github.com/zsh-users/antigen"
	ln -s -n $(ZSH_DIR)/zshenv ~/.zshenv
	ln -s -n $(ZSH_DIR)/zshrc ~/.zshrc
.PHONY: install-zsh

uninstall-zsh:
	@echo "Uninstalling Zsh config files..."
# rm -rf ~/.zsh/*
	rm ~/.zshenv || true
	rm ~/.zshrc || true
.PHONY: uninstall-zsh

# Misc Targets:

## VSCode Extensions

colorcheck:
	pastel colorcheck
.PHONY: colorcheck

fish-install-plugins:
	@echo "\n✨Preparing fish plugin manager before installing plugins..."
	brew list fisher || HOMEBREW_NO_AUTO_UPDATE=1 brew install fisher
	@echo "TBD parse ~/.config/fish/fish_plugins file, putting all plugins in a line, and provide as arg to the `fisher install <arg>`"
	fisher install edc/bass
	fisher install ilancosman/tide@v6
	fisher install jorgebucaran/nvm.fish
	fisher install sentriz/fish-pipenv

vscode-install-extensions:
	./bin/vscode/vscode-extensions-install

vscode-export-extension-list:
	./bin/vscode/vscode-extensions-list-export
.PHONY: export-vscode-extension-list
