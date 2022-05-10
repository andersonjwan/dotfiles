# Bash Shell configurations
# @author "Jacob Anderson <andersonjwan@gmail.com>"

# Inspired by the following work:
# https://github.com/mathiasbynens/dotfiles

# Load appropriate shell files.
# For each appropriate file, the following tests are conducted
# before executing:
#     1. Has read permissions
#     2. Is a regular file (e.g., not a directory)
for file in ~/.functions; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;
