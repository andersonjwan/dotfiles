#!/usr/bin/env bash
# Bootstrap the set of dotfiles.
# @author "Jacob Anderson <andersonjwan@gmail.com>"

# Inspired by the following work:
# https://github.com/mathiasbynens/dotfiles

cd "$(dirname "${BASH_SOURCE}")";

# Pull the latest changes
git pull --recurse-submodules origin main;
git submodule foreach git pull origin main;

# Update set of dotfiles and execute.
function doIt() {
    rsync --exclude "bootstrap.sh" \
          --exclude ".git/" \
          --exclude "README.md" \
          --archive --verbose --human-readable . ~;
    source ~/.bash_profile;
}

# Bootstrap dotfiles.
#
# @option --force -f Force the boostrap process carelessly
if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt;
else
    read -p "This may overwrite existing files within your home directory. Are you sure (y/n) " -n 1;
    echo "";

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt;
    fi;
fi;
