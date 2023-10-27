## fish configuration file
## aliases and prompt are kept in functions/ dir
## source: https://stackoverflow.com/a/61982376/3527499

for file in ~/.{functions*,exports*,aliases*}
  if test -r $file
    source "$file"
  end
end

if status is-interactive
    # Commands to run in interactive sessions can go here
    status --is-interactive; and rbenv init - fish | source
end

set TERM "xterm-256color"               # Sets the terminal type
set EDITOR "emacsclient -t -a ''"       # $EDITOR use Emacs in terminal
set VISUAL "emacsclient -c -a emacs"    # $VISUAL use Emacs in GUI mode

set -gx DEVELOPER ~/Developer
set -gx PRIVATE $DEVELOPER/private
set -gx PUBLIC $DEVELOPER/public

### PATH | Overriding

# Homebrew bin path
if test -d /opt/homebrew
  fish_add_path /opt/homebrew/bin
  fish_add_path /opt/homebrew/sbin
end

# ~/dotfiles macos bin
# alias ports=`$(pwd)/bin/macos/lports`
if test -d $HOME/.bin
  fish_add_path $HOME/.bin
end

# Doom Emacs
fish_add_path ~/.emacs.d/bin
alias e="emacsclient -c -a 'emacs'"

# Node.js
fish_add_path /opt/homebrew/opt/node@16/bin
set -gx LDFLAGS "-L/opt/homebrew/opt/node@16/lib"
set -gx CPPFLAGS "-I/opt/homebrew/opt/node@16/include"

# NVM
set -x NVM_DIR $HOME/.nvm
fish_add_path $NVM_DIR

# right now settle from shell by command `set --universal nvm_default_version lts`
# set -gx nvm_default_version lts

# Java
## prerequisite: brew install --cask android-studio
## HOW TO SET JAVA_HOME IN MAC OS X HIGH SIERRA WITH FISH SHELL
## https://www.alex-arriaga.com/how-to-set-java_home-in-mac-os-x-high-sierra-with-fish-shell/
export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/Contents/Home
fish_add_path $JAVA_HOME

# Android
export ANDROID_HOME=$HOME/Library/Android/sdk
fish_add_path $ANDROID_HOME/platform-tools ## add android `adb` and `fastboot` to the PATH

# bun.sh (https://bun.sh/)
fish_add_path "$HOME/.bun/bin"
# set --export BUN_INSTALL "$HOME/.bun"
# set --export PATH $BUN_INSTALL/bin $PATH

# Python 3.10
#fish_add_path ~/Library/Python/3.10/bin
