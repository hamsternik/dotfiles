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

## development env
function dev;       cd $DEVELOPER;          end
function public;    cd $PUBLIC;             end
function q;         cd $PUBLIC;             end
function private;   cd $PRIVATE;            end
function qq;        cd $PRIVATE;            end

## public projects
function work;          cd $PUBLIC/work;                    end
function b;             cd $PUBLIC/www.hamsternik.com;      end
function d;             cd $PUBLIC/dotfiles;                end
function le;            cd $PUBLIC/ledgers;                 end
