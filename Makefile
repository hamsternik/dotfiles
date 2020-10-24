SHELL := /bin/zsh
.zshrc: .SHELLFLAGS := --rcfile zshrc -ic --

bold := $(shell tput bold)
yellow := $(shell tput setaf 3)
tput_off := $(shell tput sgr0)

install:
	.dotfiles.sh --install
	make brew
.PHONY: install

uninstall:
	.dotfiles.sh --uninstall
.PHONY: uninstall

brew-core:
	brew bundle --file=brew/Brewfile-core
.PHONY: brew-core

brew-tools:
	brew bundle --file=brew/Brewfile-tools
.PHONY: brew-tools

brew-cask:
	brew bundle --file=brew/Brewfile-cask
.PHONY: brew-cask

brew:
	make brew-core
	make brew-tools
	make brew-cask
.PHONY: brew

vscode-extensions = ".vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi
.PHONY: vscode-extensions-install

antibody:
	$(info $(bold)$(yellow)Install antibody. Install all zsh packages.$(tput_off))
	$(shell ${DOTFILES}/antibody/install.sh)
.PHONY: antibody

