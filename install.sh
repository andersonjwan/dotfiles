#!/bin/bash
# This script "installs" the dotfiles in the appropriate
# locations.

# NOTE: Be sure to run this in the root directory of the
#       dotfiles project.

# "install" Emacs dotfile
ln -s $PWD/.emacs ~/.emacs
echo "Successfully installed Emacs dotfile (.emacs)"
