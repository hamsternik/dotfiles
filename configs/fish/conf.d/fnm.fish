# fnm
set FNM_PATH "/home/hamsternik/.local/share/fnm"
if [ -d "$FNM_PATH" ]
   set PATH "$FNM_PATH" $PATH
   set -l _old_xdg_runtime_dir $XDG_RUNTIME_DIR
   set -e XDG_RUNTIME_DIR
   fnm env --use-on-cd --shell fish | source
   if set -q _old_xdg_runtime_dir
      set -gx XDG_RUNTIME_DIR $_old_xdg_runtime_dir
   end
end
