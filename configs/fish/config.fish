if status is-interactive
    # Commands to run in interactive sessions can go here
    status --is-interactive; and rbenv init - fish | source
end

## PATH Overriding. Basics
set -gx DEVELOPER ~/Developer
set -gx PUBLIC $DEVELOPER/public
set -gx REMOTE $DEVELOPER/remote

## PATH Overriding. Programming Environment
set -x NVM_DIR $HOME/.nvm ## put NVM to the fish PATH

# prerequisite: brew install --cask android-studio
# HOW TO SET JAVA_HOME IN MAC OS X HIGH SIERRA WITH FISH SHELL
# https://www.alex-arriaga.com/how-to-set-java_home-in-mac-os-x-high-sierra-with-fish-shell/
export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/Contents/Home
fish_add_path $JAVA_HOME

export ANDROID_HOME=$HOME/Library/Android/sdk
fish_add_path $ANDROID_HOME/platform-tools ## add android `adb` and `fastboot` to the PATH

## Aliases. Navigation
function ..     ; cd .. ; end
function ...    ; cd ../.. ; end
function ....   ; cd ../../.. ; end
function l      ; ls -CF ; end
function la     ; ls -A ; end
function ll     ; ls -Al ; end

## Aliases. Utilities
function g      ; git $argv ; end
function cl     ; command clear ; end
function grep   ; command grep --color=auto $argv ; end
function tmux   ; command tmux -2 $argv ; end
function ip     ; curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g' ; end
function ping1  ; command ping -c 4 google.com ; end

## Aliases. Programming Environment
function dev;       cd $DEVELOPER;          end
function public;    cd $PUBLIC;             end
function private;   cd $DEVELOPER/private;  end
function remote;    cd $DEVELOPER/remote;   end

function b;         cd $PUBLIC/blog;        end
function d;         cd $PUBLIC/dotfiles;    end
function work;      cd $REMOTE/fluxon;      end

