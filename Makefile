SHELL := /bin/zsh
.zshrc: .SHELLFLAGS := --rcfile zshrc -ic --

dotfiles-install:
	${shell} ./.dotfiles.sh --install
.PHONY: dotfiles-install

dotfiles-uninstall:
	$(shell) .dotfiles.sh --uninstall
.PHONY: dotfiles-uninstall

brew-install:
	brew bundle --file=Brewfile
.PHONY: brew-install

antibody-install:
	$(shell) antibody/install.sh
.PHONY: antibody-install

vscode-extensions = "vscode/.vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi
.PHONY: vscode-extensions-install

vscode-extensions-export:
	@if [ -e "$(vscode-extensions)" ]; then code --list-extensions | xargs -L 1 echo > $(vscode-extensions); fi
.PHONY: vscode-extensions-export

# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
