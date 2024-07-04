help:
	cat Makefile

dotfiles-install:
	@./bin/env/dotfiles.sh --install
.PHONY: dotfiles-install

dotfiles-uninstall:
	@./bin/env/dotfiles.sh --uninstall
.PHONY: dotfiles-uninstall

brew-install:
	@brew bundle --file=configs/Brewfile
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

colorcheck:
	@pastel colorcheck
.PHONY: colorcheck

# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
# https://xs-labs.com/en/blog/2020/11/07/introduction-to-makefiles/
