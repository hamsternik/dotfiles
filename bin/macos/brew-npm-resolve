#!/bin/sh

# Script to fix problems with npm and node.js
# Big issue: https://github.com/npm/npm/issues/3125

rm -rf /usr/local/lib/node_modules

brew uninstall node
brew update
brew cleanup
brew install node
brew postinstall node
