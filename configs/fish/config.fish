# Navigation
function ..     ; cd .. ; end
function ...    ; cd ../.. ; end
function ....   ; cd ../../.. ; end
function l      ; ls -CF ; end
function la     ; ls -A ; end
function ll     ; ls -Al ; end

# Utilities
function g      ; git $argv ; end
function cl     ; command clear ; end
function grep   ; command grep --color=auto $argv ; end
function tmux   ; command tmux -2 $argv ; end
function ip     ; curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g' ; end
function piing  ; command ping -c 4 google.com ; end

# Developer Paths Navigation
set -gx DEVELOPER ~/Developer
set -gx PUBLIC $DEVELOPER/public
set -gx REMOTE $DEVELOPER/remote

function dev;       cd $DEVELOPER;          end
function public;    cd $PUBLIC;             end
function private;   cd $DEVELOPER/private;  end
function remote;    cd $DEVELOPER/remote;   end

function b;         cd $PUBLIC/blog;        end
function d;         cd $PUBLIC/dotfiles;    end
function work;      cd $REMOTE/fluxon;      end

if status is-interactive
    # Commands to run in interactive sessions can go here
    status --is-interactive; and rbenv init - fish | source
end

# Programming Env
set -x NVM_DIR $HOME/.nvm ## put NVM to the fish PATH
