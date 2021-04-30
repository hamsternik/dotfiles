SHELL := /bin/zsh
.zshrc: .SHELLFLAGS := --rcfile zshrc -ic --

dotfiles-install:
	${shell} ./.dotfiles.sh --install
.PHONY: install

dotfiles-uninstall:
	$(shell) .dotfiles.sh --uninstall
.PHONY: uninstall

brew-core:
	brew bundle --file=brew/Brewfile-core
.PHONY: brew-core

brew-tools:
	brew bundle --file=brew/Brewfile-tools
.PHONY: brew-tools

brew-casks:
	brew bundle --file=brew/Brewfile-casks
.PHONY: brew-cask

brew:
	make brew-core
	make brew-tools
	make brew-casks
.PHONY: brew

antibody:
	$(shell) antibody/install.sh
.PHONY: antibody

vscode-extensions = "vscode/.vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi
.PHONY: vscode-extensions-install

vscode-extensions-export:
	@if [ -e "$(vscode-extensions)" ]; then code --list-extensions | xargs -L 1 echo > $(vscode-extensions); fi
.PHONY: vscode-extensions-export


# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
