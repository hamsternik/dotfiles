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
# A syntax-highlighting pager for git and diff output
brew "git-delta"
### Grep -- GNU grep app
brew "grep"
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

### iOS App Store alternative
cask "altserver"
### Compact TeX distribution as alternative to the full TeX Live / MacTeX
cask "basictex"
### Desktop password and login vault
cask "bitwarden"
### Tool to remove unnecessary files and folders from disk
cask "cleanmymac"
### Disk space visualizer
cask "daisydisk"
### Collaborative team software
cask "figma"
### Google Font: https://fonts.google.com/specimen/Roboto
### Address svn to download fonts: https://github.com/Homebrew/homebrew-cask-fonts/issues/2039
cask "font-roboto"
### Web browser
cask "google-chrome"
### Client for the Google Drive storage service
cask "google-drive"
### JDK from Oracle
cask "oracle-jdk"
### Shows the next meeting in the menu bar
cask "meetingbar"
### Collaboration platform for API development
cask "postman"
### Move and resize windows using keyboard shortcuts or snap areas
cask "rectangle"
### Team communication and collaboration software
cask "slack"
### Messaging app with a focus on speed and security
cask "telegram"
### MS Open-source code editor
cask "visual-studio-code"
### Install and switch between multiple versions of Xcode // UI Application
cask "xcodes"

# adoptopenjdk8 fixes warning in terminal and also provides a correct jdk version for Android
# https://stackoverflow.com/questions/41993431/unable-to-find-any-jvms-matching-version-1-8-0-40-when-open-terminal-on-macos
cask "homebrew/cask-versions/adoptopenjdk8"

# Mac App Store
## https://github.com/mas-cli/mas
mas "Bear", id: 1091189122
