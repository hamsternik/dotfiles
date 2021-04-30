<h1 align="center">~/.dotfiles 💻</h1>

Hi there 👋

The basic case – your Macbook is new and so empty. And you need to settle down all basic programs, tools, configs, etc to configure as it should be actually.

## ⛏ Install

- `git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git` – first "raw" setup
- `git submodule update --init --recursive` – already downloaded repo w/o initialized submodules

## 🗿 Deploy

- open the Terminal.app (as I am preferred [alacritty](https://github.com/alacritty/alacritty) but for the right start it's enough)
- generate new ssh key, details [on the GitHub doc](https://docs.github.com/en/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- `ssh-keygen -t ed25519 -C "hamsternik9@gmail.com"`
- use the name `github-hamsternik` for public / private key
- start the ssh-agent in the background
- `eval "$(ssh-agent -s)"`
- add the ssh private key to the ssh-agent
- `ssh-add -K ~/.ssh/github-hamsternik`
- copy paste `ssh/config` on the local machine by the pass `~/.ssh/config`
- go into your [account settings](https://github.com/settings/keys) to add your public key
- `pbcopy < ~/.ssh/github-hamsternik.pub`
- download the `dotfiles` repository on your machine, clone the repo (see [Install](#-install) section)
- cd into the local repo
- install Homebrew and all my daily tools within 🍻
- `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
- `make brew-install`
- next install `antibody` for the `zsh`
- `make antibody`
- last step is to install all configs in my user's root directory
- `make dotfiles-install`
- go to the `configs/.zsh/zsh-git-prompt` to install haskell version of the tool ([more details in the README](https://github.com/hamsternik/zsh-git-prompt?organization=hamsternik&organization=hamsternik))
- `stack setup`
- `stack build && stack install`
- open vim and use `PluginInstall` command to install all the plugins

#### Troubleshooting

- error due terminal init `zsh compinit: insecure directories`

You can find the solution [here on stackoverflow](https://stackoverflow.com/questions/13762280/zsh-compinit-insecure-directories).

## 📑 Zsh Configuration

[Antibody](https://getantibody.github.io) - my main shell plugin manager for all Zsh plugins.

**All Plugins**

- [zsh-git-prompt](https://github.com/olivierverdier/zsh-git-prompt) - Informative git prompt for zsh.
- [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions) - Fish-like autosuggestions for zsh.
- [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) - Fish shell like syntax highlighting for Zsh.

### Antibody + Clean My Mac

My one and only OS is *macOS* and also I am the active user of the `CleanMyMac` app to eventually clean my disk from any sort of trash. But due to the cleaning CleanMyMac removes all antibody plugins cache 😞

After finishing basic `CleanMyMac` clean up it is a good time to re-install all Zsh plugins.

Run `make antibody` command in the root directory. Another option: open `~/dotfiles/antibody` directory and manually run `install.sh`.

## 👀 Miscellaneous

### Fonts

I'm using `spaceship-prompt` as a default Zsh prompt after a long time trying to customize it on my own. It required `zsh` to be installed and [Powerline Font](https://github.com/powerline/fonts) as well.

#### Deploy

- download the [repo](https://github.com/hamsternik/dotfiles/tree/master/fonts)
- cd to the `fonts` directory
- copy fonts to the `~/Library/Fonts` directory

### True Color Supporting

I switched from the default Terminal.app macOS application to the [cross-platform, GPU-accelerated and popular terminal emulator](https://github.com/alacritty/alacritty). Say hello, alacritty 👋🏻

The topmost reason of doing that is that [the Terminal.app does not support true colors](https://gist.github.com/XVilka/8346728#not-supporting-true-color) and I **basically can not** make my vim so much brilliant as I can do, obviously.

Right now I am using [onedark](https://github.com/joshdick/onedark.vim) colorscheme paired with the [lightline](https://github.com/itchyny/lightline.vim) which [is veeery sweety](https://www.sainnhe.dev/post/status-line-config/) yeah!

### MongoDB

Basically, I am not a cloud engineer, so I really need some space to recall the basic information about i.e. how to run
a mongoDB service localy. Here it is!

- to run MongoDB process

```
brew services start mongodb-community
```

- to stop MongoDB as macOS service
```
brew services stop mongodb-community
```

More info how to install and work with the latest version of MongoDB Community take a look in the [documentation page](https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/#run-mongodb-community-edition).
