## sashimi: fish prompt
## source: https://github.com/isacikgoz/sashimi
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
