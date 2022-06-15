#SHELL := /bin/sh
VSCODE_APP = vscodium
VSCODE_EXTENSIONS_PATH = "vscode/extensions.txt"

dotfiles-install:
	@./.dotfiles.sh --install
.PHONY: dotfiles-install

dotfiles-uninstall:
	@./.dotfiles.sh --uninstall
.PHONY: dotfiles-uninstall

brew-install:
	@brew bundle --file=Brewfile
.PHONY: brew-install

antibody-install:
	@./bin/antibody/install.sh
.PHONY: antibody-install

vscode-ext-install:
	@./bin/vscode/vscode-extensions-install
.PHONY: vscode-ext-install

vscode-ext-list-export:
	@./bin/vscode/vscode-extensions-list-export
.PHONY: vscode-ext-list-export

# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
# https://xs-labs.com/en/blog/2020/11/07/introduction-to-makefiles/
