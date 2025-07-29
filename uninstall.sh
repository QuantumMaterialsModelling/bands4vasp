#!/bin/sh
# Robust uninstall script for bands4vasp with install path detection

foldername="bands4vasp"
hdir="$HOME"
bashrc="$hdir/.bashrc"
default_ipath="$hdir/$foldername"
install_path="$default_ipath"

# 1. Try to detect from bashrc
if grep -q "#bands4vasp command" "$bashrc"; then
  pathline=`grep -A 1 "#bands4vasp command" "$bashrc" | tail -n 1`
  instdir=`echo "$pathline" | sed -n 's/export PATH="\([^"]*\)\/bin.*$/\1/p'`
  [ -n "$instdir" ] && install_path="$instdir"
fi

# 2. If still not found, ask the user
if [ ! -d "$install_path" ]; then
  echo "Could not find installation at $install_path."
  printf "Enter the path where bands4vasp is installed (or leave empty to abort): "
  read userpath
  if [ -n "$userpath" ]; then
    install_path="$userpath"
  else
    echo "Aborted."
    exit 0
  fi
fi

if [ -d "$install_path" ]; then
  echo "Removing $install_path ..."
  rm -rf "$install_path"
else
  echo "No install folder $install_path found."
fi

# Remove PATH export
if grep -q "#bands4vasp command" "$bashrc"; then
  echo "Removing PATH entry from $bashrc ..."
  ed -s "$bashrc" <<END > /dev/null
/#bands4vasp command/,+1d
w
q
END
fi

echo "Uninstall complete."
