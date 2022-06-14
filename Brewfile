# https://github.com/Homebrew/homebrew-bundle
tap "homebrew/bundle"
tap "homebrew/core"
tap "homebrew/cask"
tap "homebrew/cask-drivers"
tap "homebrew/cask-fonts"
tap "homebrew/cask-versions"
tap "mongodb/brew"
tap "dart-lang/dart"

# prerequisites

### Download with resuming and segmented downloading
### Need for `xcodes` CLI tool to download Xcodes faster
brew "aria2"

# Toolchain: iOS, macOS
brew "fastlane"
brew "rbenv"
brew "swiftformat"
brew "swiftlint"

# Toolchain: languages
brew "dart"
brew "ghc"
brew "golang"
brew "haskell-stack"
brew "kotlin"
brew "mongodb-community"
brew "node"
brew "rust", link: false
brew "yarn"

# CLI Applications
## https://github.com/Homebrew/brew/

# A cat(1) clone with wings.
brew "bat"
# Antibody -- Zsh plugin manager
brew "getantibody/tap/antibody"
# Cmake -- family of tools designed to build, test and package software
brew "cmake"
# GNU File, Shell, and Text utilities
brew "coreutils"
# User-friendly command-line shell for UNIX-like operating systems
brew "fish"
# Plugin manager for the Fish shell
brew "fisher"
# GitHub command-line tool
brew "gh"
# A syntax-highlighting pager for git and diff output
brew "git-delta"
# GNU Pretty Good Privacy (PGP) package. Formerly known as: gnupg2. See more on https://gnupg.org/
brew "gnupg"
### Grep -- GNU grep app
brew "grep"
### Easy plain text accounting with command-line, terminal and web UIs, https://hledger.org/
brew "hledger"
### HTop -- interactive process viewer for Unix systems
brew "htop"
### LF -- terminal file manager
brew "lf"
### OpenSSH -- suite of secure networking utilities based on the Secure Shell (SSH) protocol
brew "openssh"
brew "openssl@1.1" # wget dependency
# A static site generator for Swift developers
brew "publish"
### Modern replacement for ps written by Rust
brew "procs"
### Reattach -- macOS wrapper, needs for tmux
brew "reattach-to-user-namespace"
### Version control system designed to be a better CVS; `subversion` or `svn`
brew "subversion"
### Tmux -- terminal multiplexer
brew "tmux"
### Display directories as trees (with optional color/HTML output)
brew "tree"
### Install and switch between multiple versions of Xcode
brew "xcodes"
### XZ -- General-purpose data compression with high compression ratio
brew "xz"
### Watch -- Runs the specified command repeatedly and displays the results on standard output
brew "watch"
### Watch files and take action when they change
brew "watchman"

# User Applications
## https://github.com/caskroom/homebrew-cask

# adoptopenjdk8 fixes warning in terminal and also provides a correct jdk version for Android
# https://stackoverflow.com/questions/41993431/unable-to-find-any-jvms-matching-version-1-8-0-40-when-open-terminal-on-macos
cask "homebrew/cask-versions/adoptopenjdk8"

## Two-factor authentication software
cask "authy"
### iOS App Store alternative
cask "altserver"
### Compact TeX distribution as alternative to the full TeX Live / MacTeX
#cask "basictex"
### Desktop password and login vault
cask "bitwarden"
### Tool to remove unnecessary files and folders from disk
cask "cleanmymac"
### Disk space visualizer
cask "daisydisk"
### VPN client for secure internet access and private browsing
cask "expressvpn"
### Collaborative team software
cask "figma"
### Google Font: https://fonts.google.com/specimen/Roboto
### Address svn to download fonts: https://github.com/Homebrew/homebrew-cask-fonts/issues/2039
cask "font-roboto"
# Desktop client for GitHub repositories
cask "github"
### Web browser
cask "google-chrome"
### Client for the Google Drive storage service
cask "google-drive"
# Interface for reading and syncing eBooks
cask "kindle"
### JDK from Oracle
cask "oracle-jdk"
### Shows the next meeting in the menu bar
cask "meetingbar"
### TeX distribution
cask "miktex-console"
### Collaboration platform for API development
cask "postman"
# Modern and intuitive HTTP Debugging Proxy app
cask "proxyman"
### Move and resize windows using keyboard shortcuts or snap areas
cask "rectangle"
## Tool that provides consistent, highly configurable symbols for apps
cask "sf-symbols"
### Team communication and collaboration software
cask "slack"
### Messaging app with a focus on speed and security
cask "telegram"
### MS Open-source code editor
cask "visual-studio-code"
### Install and switch between multiple versions of Xcode // UI Application
cask "xcodes"
# Video communication and virtual meeting platform
cask "zoom"
## Collect, organize, cite, and share research sources
cask "zotero"
## Toggl Time tracker
cask "toggl-track"

# Miscellaneous Applications

## Tools for building Android applications
# cask "android-studio"
## E-books management software
# cask "calibre"
## Tool to run Windows software
# cask "crossover"
## Open-source software for live streaming and screen recording
# cask "obs"

# Mac App Store
## https://github.com/mas-cli/mas
mas "Bear", id: 1091189122

# Flow. A pomodoro based work timer to help you get things done
mas "Flow", id: 1423210932

# Flexible boards for deep work
mas "Muse", id: 1501563902

## Logitec G Hub. Mouse Application, more at https://www.logitechg.com/en-eu/innovation/g-hub.html
# wget "https://download01.logi.com/web/ftp/pub/techsupport/gaming/lghub_installer.zip" && unzip "lghub_installer.zip"

## Online translation based on NMT (Neural Machine Translation), contextual dictionaries, online bilingual concordances, etc
# wget "https://dl.reverso.net/desktop-app/macos" && open Reverso.dmg
