if status is-interactive
    # Commands to run in interactive sessions can go here
    status --is-interactive; and rbenv init - fish | source
end

set TERM "xterm-256color"               # Sets the terminal type
set EDITOR "emacsclient -t -a ''"       # $EDITOR use Emacs in terminal
set VISUAL "emacsclient -c -a emacs"    # $VISUAL use Emacs in GUI mode

set -gx DEVELOPER ~/Developer
set -gx PUBLIC $DEVELOPER/public
set -gx REMOTE $DEVELOPER/remote

### PATH | Overriding

# Doom Emacs
fish_add_path ~/.emacs.d/bin
alias e="emacsclient -c -a 'emacs'"

# Node.js
fish_add_path /opt/homebrew/opt/node@16/bin
set -gx LDFLAGS "-L/opt/homebrew/opt/node@16/lib"
set -gx CPPFLAGS "-I/opt/homebrew/opt/node@16/include"

# NVM
set -x NVM_DIR $HOME/.nvm ## put NVM to the fish PATH
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
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

# Python 3.10
fish_add_path ~/Library/Python/3.10/bin

### Aliases

# Navigation
function ..     ; cd .. ;               end
function ...    ; cd ../.. ;            end
function ....   ; cd ../../.. ;         end
function l      ; ls -CF ;              end
function la     ; ls -A ;               end
function ll     ; ls -Al ;              end

# Utilities
function g      ; git $argv ;           end
function cl     ; command clear ;       end
function grep   ; command grep --color=auto $argv ;     end
function tmux   ; command tmux -2 $argv ;               end
function ip     ; curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g' ; end
function ping1  ; command ping -c 4 google.com ;        end

# Work Environment
function dev;       cd $DEVELOPER;          end
function public;    cd $PUBLIC;             end
function private;   cd $DEVELOPER/private;  end
function remote;    cd $DEVELOPER/remote;   end

# Most Active Projects
function work;      cd $REMOTE/work;                        end
function d;         cd $PUBLIC/dotfiles;                    end
function b;         cd $PUBLIC/www.hamsternik.com;          end

### Fish Prompt
# name: sashimi
function fish_prompt
  set -l last_status $status

  set -l red (set_color red)
  set -l green (set_color green)
  set -l blue (set_color blue)
  set -l cyan (set_color cyan)
  set -l yellow (set_color yellow)
  set -l magenta (set_color magenta)
  set -l normal (set_color normal)

  set -g whitespace ' '

  set -l ahead (_git_ahead)
  set -l user "$blue$USER"
  set -l at {$normal}"at"
  set -l on {$normal}"on"

  if test $last_status = 0
    set status_indicator "$normal❯$cyan❯$green❯"
  else
    set status_indicator "[$last_status] $red❯$red❯$red❯"
  end

  set -l pwd_prefix $green(basename (prompt_pwd))

  if [ (_git_branch_name) ]
    set -l branch (_git_branch_name)
    if test "$branch" = 'master' -o "$branch" = 'main'
      set -l git_branch (_git_branch_name)
      set git_info "$on $red$git_branch$normal"
    else
      set -l git_branch (_git_branch_name)
      set git_info "$on $magenta$git_branch$normal"
    end

    if [ (_is_git_dirty) ]
      set -l dirty "$yellow ✗"
      set git_info "$git_info$dirty"
    end
  end

  # Notify if a command took more than 5 minutes
  if [ "$CMD_DURATION" -gt 300000 ]
    echo The last command took (math "$CMD_DURATION/1000") seconds.
  end

  echo -n -s $user $whitespace \
    $at $whitespace \
    $pwd_prefix $whitespace \
    $git_info $whitespace \
    $ahead $status_indicator $whitespace
end

function _git_ahead
  set -l commits (command git rev-list --left-right '@{upstream}...HEAD' 2>/dev/null)
  if [ $status != 0 ]
    return
  end
  set -l behind (count (for arg in $commits; echo $arg; end | grep '^<'))

  set -l ahead  (count (for arg in $commits; echo $arg; end | grep -v '^<'))
  switch "$ahead $behind"
    case ''     # no upstream
    case '0 0'  # equal to upstream
      return
    case '* 0'  # ahead of upstream
      echo "$blue↑$normal_c$ahead$whitespace"
    case '0 *'  # behind upstream
      echo "$red↓$normal_c$behind$whitespace"
    case '*'    # diverged from upstream
      echo "$blue↑$normal$ahead $red↓$normal_c$behind$whitespace"
  end
end

function _git_branch_name
  echo (command git symbolic-ref HEAD 2>/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty 2>/dev/null)
end

