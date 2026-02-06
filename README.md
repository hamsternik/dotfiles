<h1 align="center">~/.dotfiles 💻</h1>

Hi there 👋

## Before clone (Settle SSH Keys)

The basic case – your Macbook is new and so empty. And you need to settle down all basic programs, tools, configs, etc to configure as it should be actually.

First, install the **Homebrew** on macOS:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Visit [the official website](https://brew.sh/) if there are any errors occures with the curl command or bash script.

Second, deal with SSH keys before clone the dotfiles repo. Copy this into the `~/.ssh/config` file:

```bash
Host *
    AddKeysToAgent yes
    IgnoreUnknown UseKeychain
    UseKeychain yes
Host github.com*
    User git
    HostName github.com
    IdentityFile ~/.ssh/github-hamsternik
Host codeberg.org
    User git
    HostName codeberg.org
    IdentityFile ~/.ssh/codeberg-hamsternik
```

Generate new local ssh key for the personal email. Use `github-hamsternik` key name.

```bash
ssh-keygen -t ed25519 -C "hamsternik9@gmail.com"
```

Give the name accordingly to the platform the key pair will target,
e.g. for GitHub use 'github-*' prefix, for Codeberg use 'codeberg-*' one.

Copy the public key into the clipboard. E.g. use `pbcopy` on macOS. Or
just use `cat` on Linux/WSL to print out *.pub key and copy from the
terminal.

- `pbcopy < ~/.ssh/github-hamsternik.pub` OR
- `cat ~/.ssh/github-hamsternik.pub`

Settle the public key into your profile settings:
- https://githib.com/settings/keys for GitHub OR
- https://codeberg.org/user/settings/keys for Codeberg

## Deploy

Clone the repo and jump into it:

```bash
git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git && cd dotfiles
```

or if you already cloned the repo without extra key for to load submodules, download all of submodules first:

```bash
git submodule update --init --recursive
```

Install config files in the users' root directory. 
Just FYI – you are using Zsh at this moment. But don't worry, you will migrate to the `fish` the next turn.

```bash
make dotfiles-install
```

Next you need to settle the only `zsh-git-prompt` plugin I am using today with Zsh configuration.
There are 2 versions of this plugin and my choice (and author's one) is the haskell version, of course.
To set it up, first go and download the latest version of the haskell tool stack called `stack`.

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Then jump to the git plugin directory and use next commands:

```bash
stack setup
```

Please wait until the `setup` is finished to install the proper version of the `ghci` and other tools.
Then run:

```bash
stack build && stack install
```

Next come back to the user root directory and cd into `.vim/bundle`. Run:

```bash
git clone git@github.com:VundleVim/Vundle.vim.git
```

Install all plugins described into the [.vimrc config file](/configs/vim/vimrc). 
Open vim and run `PluginInstall`.
Vundle will handle all the work automatically.

The last step is to install applications and CLI tools via the Homebrew.
As you see, I am actively using `make` so it would be easy to you to run any command to set up the machine "one click".

```
make brew-install
```

## Misc: Configs

### GPG

First, to disable signing your git commits just off the `commit.gpgsign` option into your git configuration file.
That helps to eliminate any commit issues related the older signing.

To be able to commit with `commit.gpgsign true` setting in your git configuration, you have to set up GPG unique key on the machine.
Check more information about signing commits and git configuration on the [official GitHub doc page](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits).

To generate new GPG key you should install `gnupg` command-line tool first. Verify whether you have the app on your machine after full Homebrew settings file installation. Otherwithe use `$ brew install gnupg` command to install the app first. Then open the terminal. Generate a GPG key pair.

```bash
gpg --default-new-key-algo rsa4096 --gen-key
```

This command is the new way to pass some arguments and omit extra work by-default. 
The plain version of the command check out at the [Generating a new GPG key](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key) doc page.

Enter your user-ID information.
I am using `hamsternik` GitHub account username for that.
Next, use gh account's email.

For more information about email verification on GitHub check [this doc](https://docs.github.com/en/get-started/signing-up-for-github/verifying-your-email-address) out.

⚠️ After couple attempts to create GPG key with passphrase `git` does not commit any changes from any public repo.
Idk what is the reason. For now I have GPG key with 2 years of expiration date w/o passphrase.

Use the next command to list the long form of the GPG keys for which you have both a public and private key:

```bash
gpg --list-secret-keys --keyid-format=long
```

From the list of GPG keys, copy the long form of the GPG key ID you'd like to use.
E.g. in line `sec 4096R/3AA5C34371567BD2 2016-03-10 [expires: 2017-03-10]` the long form of the GPG generated key is `3AA5C34371567BD2` part.
Copy that text from the terminal and insert into your `.gitconfig` file. You can find configuration file by the `configs/gitconfig` path.
Run to copy the public GPG key:

```bash
gpg --armor --export CCD4FA8B4F35837031CBFD0E9474253FF8C3327E-EXAMPLE | pbcopy
```

Paste copied public key on [GPG keys page](https://github.com/settings/keys) on GitHub.

The last step to do is telling `gh` to refresh new GPG key(s) locally.
Run the command:

```bash
gh auth refresh -s read:gpg_key
```

#### Troubleshooting.

If you have any questions please refer [the official GitHub page](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key).

1/ When commit changes in any git repo the next error provided down below.
TODO: figure out what is the reason.

```bash
error: gpg failed to sign the data
fatal: failed to write commit object
```

### Fish shell

In 2022 I had switched from macOS default shell 'Zsh' to the brand-new eye-candy [fish shell](https://fishshell.com).
Before I had Zsh with custom configuration and small set of scripts onboard, but that is all a history right now.
The one thing you have to do is to install `fish` via Homebrew where the `brew-install` command already did that for you!

There are multiple things that need to be comprehended, but SO so blessed to speed the process up.
Here are some answers how to deal with $PATH variables and aliases:

- [Modifying PATH with fish shell [closed]. Command to add / modify PATH vars](https://stackoverflow.com/a/34401308/3527499);
- [Modifying PATH with fish shell [closed]. Basic variables for popular programming languages & tools](https://stackoverflow.com/a/63655568/3527499);

Furthermore, there are some official document pages from fish shell addressing issues:
- [add path variable in fish shell](https://fishshell.com/docs/current/cmds/fish_add_path.html);
- [alias -- create a function](https://fishshell.com/docs/current/cmds/alias.html?highlight=alias);

I am using [fisher](http://git.io/fisher) fish plugin manager to handle all my programming stuff around.
See below certain plugins.

### LaTeX

My [resume](https://github.com/hamsternik/cv) has been written using LaTeX to process plain text into representable peace of my experience and career.
I am not a big fan of [huge MacTex](https://tug.org/mactex/mactex-download.html) package. Official source says about ~5Gb of data. Big Yikes!

Let's install [A Smaller Distribution, Basic TeX](https://tug.org/mactex/morepackages.html).
Nowadays `basictex` brew package will not be installed automatically with the whole Brewfile apps batch.

```bash
brew install basictex --cask
```

I am using `latexmk` Perl script to automate the assembly process for the cv repo. 
Check out more about [the latexmk documentation](https://mg.readthedocs.io/latexmk.html).
To install `latextmk` use `tlmgr` provided in the `basictex` package:

```bash
sudo tlmgr install latexmk
```

Most of the time the prompt will say you need update `tlmgr` right after.

```bash
sudo tlmgr update --self
```

That is it. Go to the [cv repo](https://github.com/hamsternik/cv) and check it out!


### Node.js and NVM

Brew officially provide [node.js](https://formulae.brew.sh/formula/node#default) and [NVM](https://formulae.brew.sh/formula/nvm#default) as node.js versions manager.
But unfortunatelly for any kind of shell you have settled you need to make extra steps to run nvm up the right way.
To enable NVM for fish you need to export NVM_DIR into your PATH, e.g. `$fish_complete_path` variable.
Another thing I have done to load `nvm` dynamically on each shell session is to load NVM somehome.
I am using [nvm.fish](git.io/nvm.fish) plugin for that purpose because it handles all the things around for me.

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

There is a [discussion thread](https://discussions.apple.com/thread/250636611?sortBy=best) on the Apple forum about 24bit True Color support in macOS Terminal.app. The short answer: NO.

## Emacs

The whole Emacs installation & system configuration I got by these links

- [Doom Emacs. Installation section](https://github.com/doomemacs/doomemacs#install)
- [Emacs Deamon Mode on macos](https://briansunter.com/blog/emacs-daemon-macos/)

To install Emacs Client with Doom configuration framework just run these 2 commands and give 3-5 min:

```sh
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

To launch `emacs` daemon within every user login create the next file by the pass
`~/Library/LaunchAgents/gnu.emacs.daemon.plist`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>gnu.emacs.daemon</string>
  <key>ProgramArguments</key>
  <array>
    <string>/opt/homebrew/bin/emacs</string>
    <string>--daemon</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>ServiceDescription</key>
  <string>GNU Emacs Daemon</string>
</dict>
</plist>
```

and use `launchctl` commands to load the script due the startup:

```sh
launchctl unload ~/Library/LaunchAgents/gnu.emacs.daemon.plist
launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
```

**[UPD]** Both of these commands are [deprecated by Apple starting from 10.10](https://babodee.wordpress.com/2016/04/09/launchctl-2-0-syntax/).
Instead use new `bootstrap` and `bootout` commands with appropriate user's UID and service-name:

``` bash
id -u "Nikita Khomitsevych" # To know current user UID
sudo launchctl bootstrap gui/<user's UID> ~/Library/LaunchAgents/gnu.emacs.daemon.plist # Use `sudo` to get more detailed error
```

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
