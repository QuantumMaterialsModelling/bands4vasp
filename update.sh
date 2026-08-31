#!/bin/sh
# Robust update script for bands4vasp with install path detection

foldername="bands4vasp"
hdir="$HOME"
bashrc="$hdir/.bashrc"
default_ipath="$hdir/$foldername"
install_path="${BANDS4VASP_INSTALL_PATH:-$default_ipath}"
explicit_path="${BANDS4VASP_INSTALL_PATH:-}"

# 1. Try to detect from bashrc
if [ -z "${BANDS4VASP_INSTALL_PATH:-}" ] && grep -q "#bands4vasp command" "$bashrc"; then
  pathline=`grep -A 1 "#bands4vasp command" "$bashrc" | tail -n 1`
  # extract path from export line
  # expects: export PATH="/some/path/bin:$PATH"
  instdir=`echo "$pathline" | sed -n 's/export PATH="\([^"]*\)\/bin.*$/\1/p'`
  [ -n "$instdir" ] && install_path="$instdir"
fi

# 2. If still not found, ask the user
if [ ! -d "$install_path" ] && [ -z "$explicit_path" ]; then
  echo "Could not find installation at $install_path."
  printf "Enter the path where bands4vasp is installed: "
  read userpath
  [ -n "$userpath" ] && install_path="$userpath"
fi

backup_path=""
if [ -d "$install_path" ]; then
  backup_path="${install_path}.update-backup.$$"
  echo "Moving previous installation to temporary backup $backup_path ..."
  mv "$install_path" "$backup_path" || exit 1
else
  echo "No installation found at $install_path. Installing fresh."
fi

echo "Starting fresh installation ..."
cd "`dirname "$0"`" || exit 1
BANDS4VASP_INSTALL_PATH="$install_path"
export BANDS4VASP_INSTALL_PATH
if sh ./install.sh; then
  if [ -n "$backup_path" ] && [ -d "$backup_path" ]; then
    rm -rf "$backup_path"
  fi
  echo "Update completed successfully."
else
  status=$?
  echo "ERROR: Installation failed."
  if [ -n "$backup_path" ] && [ -d "$backup_path" ]; then
    echo "Restoring previous installation ..."
    rm -rf "$install_path"
    mv "$backup_path" "$install_path"
  fi
  exit "$status"
fi
