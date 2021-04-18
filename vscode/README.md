# VSCode Configuration

## Physical location of files

MacOS. Tested on macOS Big Sur

- `~/Library/Application\ Support/Code/User/keybindings.json`
- `~/Library/Application\ Support/Code/User/settings.json`

You're able to create a [symbolic link](https://ru.wikipedia.org/wiki/Ln_(Unix)) on each file from the `.dotfiles` directory for

- keybindings.json

```bash
ln -v -s $DOTFILES/vscode/keybindings.json ~/Library/Application\ Support/Code/User/keybindings.json
```

- settings.json

```bash
ln -v -s $DOTFILES/vscode/settings.json ~/Library/Application\ Support/Code/User/settings.json
```

## References

- [vscode + vim: commenting line doesn't move cursor down (stackoverflow)](https://stackoverflow.com/questions/52406245/vscode-vim-commenting-line-doesnt-move-cursor-down/67149695#67149695)