#!/usr/bin/env bash
# Bootstrap the set of dotfiles.
# @author "Jacob Anderson <andersonjwan@gmail.com>"

# Inspired by the following work:
# https://github.com/mathiasbynens/dotfiles

cd "$(dirname "${BASH_SOURCE}")";

# Update set of dotfiles and execute.
function doIt() {
    rsync --exclude "bootstrap.sh" \
          --exclude ".bashrc" \
          --exclude ".git/" \
          --exclude ".gitmodules" \
          --exclude "README.md" \
          --archive --verbose --human-readable . ~;

    echo "" >> ~/.bashrc;
    cat ".bashrc" >> ~/.bashrc;
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
