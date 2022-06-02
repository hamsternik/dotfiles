<h1 align="center">~/.dotfiles 💻</h1>

Hi there 👋

The basic case – your Macbook is new and so empty. And you need to settle down all basic programs, tools, configs, etc to configure as it should be actually.

# Clone

- `git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git` – first "raw" setup
- `git submodule update --init --recursive` – already downloaded repo w/o initialized submodules

# Deploy

### before clone

- open preferred terminal (you use daily)
- generate new ssh key with concrete email: `$ ssh-keygen -t ed25519 -C "hamsternik9@gmail.com"`
- for more information about SSH generation see details [on the GitHub doc](https://docs.github.com/en/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- use the name `github-hamsternik` for public / private key
- start the ssh-agent in the background: `$ eval "$(ssh-agent -s)"`
- add the ssh private key to the ssh-agent: `$ ssh-add -K ~/.ssh/github-hamsternik`
- copy `ssh/config` text from the repo to your local machine by the `~/.ssh/config` path
- go into your [account settings](https://github.com/settings/keys) to add your public key
- save into the buffer public ssh key text: `$ pbcopy < ~/.ssh/github-hamsternik.pub`

### tools setup

- clone the repo (first setup): `$ git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git`
- install Homebrew on the pretty fresh machine: `$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
- install all tools and application within Homebrew (via Make command): `$ make brew-install`
- next install `antibody` -- pluggin manager for Zsh written on Go (via Make command): `$ make antibody`
- install all configs in the user's root directory: `$ make dotfiles-install`
- create `~/.zsh` and `~/.vim/bundle` directories manually (will automate due the [#29](https://github.com/hamsternik/dotfiles/issues/29) and [#30](https://github.com/hamsternik/dotfiles/issues/30) issues)
- go to the `~/.zsh` dir and run: `$ git clone git@github.com:hamsternik/zsh-git-prompt.git`
- `cd` into the `zsh-git-prompt` and run 2 commands ([read more details](https://github.com/hamsternik/zsh-git-prompt?organization=hamsternik&organization=hamsternik))
- `$ stack setup`
- `$ stack build && stack install`
- go to the `~/.vim/bundle` and run `$ git clone git@github.com:VundleVim/Vundle.vim.git`
- open vim and use `PluginInstall` command to install all the plugins

If you see such zsh error:

> error due terminal init `zsh compinit: insecure directories`

Please check out this [stackoverflow solution](https://stackoverflow.com/questions/13762280/zsh-compinit-insecure-directories).

### configs setup

To be able to commit with `commit.gpgsign true` setting in your git configuration, you have to set up GPG unique key on the machine.
Check more information about signing commits and git configuration on the [official GitHub doc page](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits).

To generate new GPG key you should install `gnupg` command-line tool first. Verify whether you have the app on your machine after full Homebrew settings file installation. Otherwithe use `$ brew install gnupg` command to install the app first. Then open the terminal. Generate a GPG key pair.

```bash
$ gpg --full-generate-key
```

Your key must be at least 4096 bits.
Enter your user ID information. I used `hamsternik` GitHub account username for that.
Use your GitHub account verified email. For more information about email verification on GitHub check [this doc](https://docs.github.com/en/get-started/signing-up-for-github/verifying-your-email-address) out.
Type a secure passphrase... but I don't use any passphrases right now actually 🤷🏻‍♂️
Use the next command to list the long form of the GPG keys for which you have both a public and private key:

```bash
$ gpg --list-secret-keys --keyid-format=long
```

From the list of GPG keys, copy the long form of the GPG key ID you'd like to use. E.g. in line `sec   4096R/3AA5C34371567BD2 2016-03-10 [expires: 2017-03-10]` the long form of the GPG generated key is `3AA5C34371567BD2` part. Copy that text from the terminal and insert into your `.gitconfig` file. You can find configuration file by the `configs/gitconfig` path.

All the details about signing generated GPG key see on [official GitHub page](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key).

To disable signing your git commits just off the `commit.gpgsign` option into your git configuration file.
More information about that see on [official GitHub page](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits).

## Fish shell

In 2022 I had switched from macOS default shell 'Zsh' to the brand-new eye-candy [fish shell](https://fishshell.com).
Before I had Zsh with custom configuration and small set of scripts onboard, but that is all a history right now.
The one thing you have to do is to install `fish` via Homebrew where the `brew-install` command already did that for you!

## Fonts

I'm using `spaceship-prompt` as a default Zsh prompt after a long time trying to customize it on my own. It required `zsh` to be installed and [Powerline Font](https://github.com/powerline/fonts) as well.

Steps to Deploy:

- download the [repo](https://github.com/hamsternik/dotfiles/tree/master/fonts)
- cd to the `fonts` directory
- copy fonts to the `~/Library/Fonts` directory

## True Color Supporting

I switched from the default Terminal.app macOS application to the [cross-platform, GPU-accelerated and popular terminal emulator](https://github.com/alacritty/alacritty). Say hello, alacritty 👋🏻

The topmost reason of doing that is that [the Terminal.app does not support true colors](https://gist.github.com/XVilka/8346728#not-supporting-true-color) and I **basically can not** make my vim so much brilliant as I can do, obviously.

Right now I am using [onedark](https://github.com/joshdick/onedark.vim) colorscheme paired with the [lightline](https://github.com/itchyny/lightline.vim) which [is veeery sweety](https://www.sainnhe.dev/post/status-line-config/) yeah!

## MongoDB

Basically, I am not a cloud engineer, so I really need some space to recall the basic information about i.e. how to run
a mongoDB service localy. Here it is!

- to run MongoDB process

```bash
$ brew services start mongodb-community
```

- to stop MongoDB as macOS service

```bash
$ brew services stop mongodb-community
```

More info how to install and work with the latest version of MongoDB Community take a look in the [documentation page](https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/#run-mongodb-community-edition).

## SourceKit-LSP ❤️ VSCode

The SourceKit-LSP server is included with the Swift toolchain. Currently my single workflow is using the latest Xcode app within macOS.
Starting from Xcode 11.4+ `sourcekit-lsp` is included as the part of Swift toolchain.

The extension stored in the repo, follow the `vscode/extensions` path and run

```sh
code --install-extension sourcekit-lsp-development.vsix
```

Launch the VSCode with any Xcode project to verify wether `sourcekit-lsp` works without errors.
Then open `Settings (cmd+,) -> Extensions -> SourceKit-LSP` and configure both the `Server Path` and `Toolchain Path`.
Example of predefined settings for the plugin in the `vscode/settings.json`.

Unfortunately, `sourcekit-lsp` does not support standard iOS projects, including these I'm working day-to-day on my work. The pipeline to tackle Swift project together with LSP is using Swift Package Manager to build the project. If you have such type of project, e.g. CLI application builded on Swift just run `swift build` in terminal / VSCode.

To know more about what's sourcekit-lsp and how it can be bind together with VSCode follow the [official README on sourcekit-lsp GitHub page](https://github.com/apple/sourcekit-lsp).

### [OPTIONAL] How to build VSCode extension from scratch?

Download the [sourcekit-lsp repo](https://github.com/apple/sourcekit-lsp) first. As I'm have `sourcekit-lsp` already as the part of Xcode toolchain the only thing what I need to do is build sourcekit-lsp extension for VScode. In terminal go into `sourcekit-lsp/Editors/vscode` directory and run the following commands to build the VSCode extension.

```bash
$ cd Editors/vscode
$ npm install
$ npm run dev-package
```

Install the package from the command-line

```bash
$ code --install-extension sourcekit-lsp-development.vsix
```
