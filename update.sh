#!/bin/sh
# Robust update script for bands4vasp with install path detection

foldername="bands4vasp"
hdir="$HOME"
bashrc="$hdir/.bashrc"
default_ipath="$hdir/$foldername"
install_path="$default_ipath"

# 1. Try to detect from bashrc
if grep -q "#bands4vasp command" "$bashrc"; then
  pathline=`grep -A 1 "#bands4vasp command" "$bashrc" | tail -n 1`
  # extract path from export line
  # expects: export PATH="/some/path/bin:$PATH"
  instdir=`echo "$pathline" | sed -n 's/export PATH="\([^"]*\)\/bin.*$/\1/p'`
  [ -n "$instdir" ] && install_path="$instdir"
fi

# 2. If still not found, ask the user
if [ ! -d "$install_path" ]; then
  echo "Could not find installation at $install_path."
  printf "Enter the path where bands4vasp is installed: "
  read userpath
  [ -n "$userpath" ] && install_path="$userpath"
fi

if [ -d "$install_path" ]; then
  echo "Removing previous installation at $install_path ..."
  rm -rf "$install_path"
else
  echo "No installation found at $install_path. Installing fresh."
fi

echo "Starting fresh installation ..."
cd "`dirname "$0"`" || exit 1
exec ./install.sh

