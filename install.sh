#!/bin/sh
# installer for bands4vasp (files, Fortran, python venv)

set -eu

foldername="bands4vasp"
commandname="b4vasp"
pyreqs="numpy matplotlib"
hdir="$HOME"
bashrc="$hdir/.bashrc"

echo "Required: gfortran, gnuplot, ghostscript"
printf "Press ENTER to continue or type 'exit' to abort: "
read answ
case "$answ" in
  exit|EXIT|Exit) exit 1 ;;
esac

# Installation path dialog. update.sh passes the existing custom path through
# BANDS4VASP_INSTALL_PATH so an update cannot accidentally move the install.
ipath="${BANDS4VASP_INSTALL_PATH:-}"
while [ -z "$ipath" ]; do
  printf "Enter installation path or press ENTER for %s/%s: " "$hdir" "$foldername"
  read entered_path
  [ -z "$entered_path" ] && entered_path="$hdir/$foldername"
  ipath="$entered_path"
  case "$ipath" in
    "~"*) ipath="$hdir/`echo "$ipath" | sed 's/^~//'`" ;;
  esac
  if [ ! -d "$ipath" ]; then
    printf "%s does not exist. Create it? [y/N]: " "$ipath"
    read yn
    case "$yn" in
      y|Y|"") mkdir -p "$ipath" || exit 1 ;;
      *) ipath=""; continue ;;
    esac
  fi
done
if [ ! -d "$ipath" ]; then
  mkdir -p "$ipath" || exit 1
fi
echo "Installation directory: $ipath"
mkdir -p "$ipath/bin"

# Optional: Overwrite previous installation
if [ -e "$ipath/.install_marker" ]; then
  printf "WARNING: Previous installation detected. Overwrite? [y/N]: "
  read yes
  case "$yes" in
    y|Y|"") rm -rf "$ipath"/* ;;
    *) exit 1 ;;
  esac
fi

# Install the edited working-tree package when available. Fall back to a
# release tarball for distributions that do not include the source directory.
fpath="$(cd "$(dirname "$0")" && pwd)"
source_dir=`find "$fpath" -maxdepth 1 -type d -name 'bands4vasp_v*' | sort | tail -n1`
tarfile=`find "$fpath" -maxdepth 1 -type f -name 'bands4vasp_v*.tar.gz' | sort | tail -n1`
if [ -n "$source_dir" ]; then
  echo "Copying sources from $source_dir ..."
  cp -R "$source_dir/." "$ipath/"
  install_source=`basename "$source_dir"`
elif [ -n "$tarfile" ]; then
  tar xfvz "$tarfile" -C "$ipath"
  install_source=`basename "$tarfile"`
else
  echo "ERROR: No bands4vasp source directory or release tarball found."
  exit 1
fi


# PATH export
if ! grep -q "#bands4vasp command" "$bashrc" 2>/dev/null; then
  printf "\n#bands4vasp command\nexport PATH=\"%s/bin:\$PATH\"\n" "$ipath" >> "$bashrc"
  echo "'b4vasp' command added to PATH in $bashrc."
else
  # Replace line if path changes
  # POSIX ed(1) instead of GNU sed -i for compatibility
  tmped="tmpedfile_$$"
  ed -s "$bashrc" <<END > /dev/null
/#bands4vasp command/+1c
export PATH="$ipath/bin:\$PATH"
.
w
q
END
fi
chmod +x $ipath/bin/b4vasp $ipath/bin/*.sh

#############################################################################
# --------- Fortran build ---------
echo "Compiling Fortran programs ..."
cd "$ipath/src/" || exit 1
gfortran -c math.f90 || exit 1
gfortran math.f90 -c mylattice.f90 || exit 1
gfortran math.f90 -c ebs_typs.f90 || exit 1
gfortran math.f90 ebs_typs.f90 mylattice.f90 -c ebs_methods.f90 || exit 1
gfortran -g -fcheck=all -Wall math.o mylattice.o ebs_typs.o ebs_methods.o ebs_main.f90 -o nebsfitting || exit 1
gfortran math.o getradlines.f90 -o getradlines4vasp || exit 1
gfortran getsurfacelines.f90 -o getsurfacelines4vasp || exit 1

mv -f getradlines4vasp "$ipath/bin/" || exit 1
mv -f getsurfacelines4vasp "$ipath/bin/" || exit 1
mv -f nebsfitting "$ipath/bin/" || exit 1
cd - > /dev/null || exit 1

#############################################################################
# --------- Python venv ---------
echo "Setting up Python venv (for plotting tools) ..."
PYTHON_BIN="python3"
if command -v "$PYTHON_BIN" >/dev/null 2>&1; then
  "$PYTHON_BIN" -m venv "$ipath/venv"
  VENV_PYTHON="$ipath/venv/bin/python"
  "$VENV_PYTHON" -m pip install --upgrade pip
  "$VENV_PYTHON" -m pip install $pyreqs
  echo "Python venv ready in $ipath/venv"
  echo "Activate with: . \"$ipath/venv/bin/activate\""
else
  echo "WARNING: python3 not found! Python tools will not work."
fi

# Mark installation
echo "$install_source" > "$ipath/.install_marker"

echo
echo ">>>>>>>>>> INSTALLATION COMPLETE <<<<<<<<<<"
echo "Type 'b4vasp --help' for usage."
printf "Reload bash so the new PATH is active? [yes/no]: "
read ans
case "$ans" in
  yes|YES|Yes) exec bash ;;
esac
