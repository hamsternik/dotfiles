#!/bin/bash

# Install: Setup `rbenv` for normal ruby setup 
rbenv install 2.4.3
rbenv global 2.4.3
rbenv rehash
rbenv init -
ruby -v

