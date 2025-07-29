#!/bin/sh
# installer for bands4vasp (files, Fortran, python venv)

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

# Installation path dialog
while :; do
  printf "Enter installation path or press ENTER for %s/%s: " "$hdir" "$foldername"
  read ipath
  [ -z "$ipath" ] && ipath="$hdir/$foldername"
  case "$ipath" in
    "~"*) ipath="$hdir/`echo "$ipath" | sed 's/^~//'`" ;;
  esac
  if [ ! -d "$ipath" ]; then
    printf "%s does not exist. Create it? [y/N]: " "$ipath"
    read yn
    case "$yn" in
      y|Y|"") mkdir -p "$ipath" || exit 1 ;;
      *) continue ;;
    esac
  fi
  break
done
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

# Locate and extract release tarball
fpath="$(cd "$(dirname "$0")" && pwd)"
tarfile=`ls "$fpath" | grep -E '^bands4vasp_v.*\.tar\.gz$' | tail -n1`
if [ -z "$tarfile" ]; then
  echo "Error: No bands4vasp-*.tar.gz found!"
  exit 1
fi
tar xfvz "$fpath/$tarfile" -C "$ipath"

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

#############################################################################
# --------- Fortran build ---------
echo "Compiling Fortran programs ..."
cd "$ipath/src/" || exit 1
gfortran -c math.f90 || exit 1
gfortran math.f90 -c mylattice.f90 || exit 1
gfortran math.f90 -c ebs_typs.f90 || exit 1
gfortran math.f90 ebs_typs.f90 mylattice.f90 -c ebs_methods.f90 || exit 1
gfortran -g -fcheck=all -Wall math.o mylattice.o ebs_typs.o ebs_methods.o ebs_main.f90 -o nebsfitting || exit 1
gfortran getradlines.f90 -o getradlines4vasp || exit 1

mv -f getradlines4vasp "$ipath/bin/" || exit 1
mv -f nebsfitting "$ipath/bin/" || exit 1
cd - > /dev/null || exit 1

#############################################################################
# --------- Python venv ---------
echo "Setting up Python venv (for plotting tools) ..."
PYTHON_BIN="python3"
if command -v "$PYTHON_BIN" >/dev/null 2>&1; then
  "$PYTHON_BIN" -m venv "$ipath/venv"
  . "$ipath/venv/bin/activate"
  pip install --upgrade pip
  pip install $pyreqs
  deactivate
  echo "Python venv ready in $ipath/venv"
  echo "Activate with: . \"$ipath/venv/bin/activate\""
else
  echo "WARNING: python3 not found! Python tools will not work."
fi

# Mark installation
echo "$tarfile" > "$ipath/.install_marker"

echo
echo ">>>>>>>>>> INSTALLATION COMPLETE <<<<<<<<<<"
echo "Type 'b4vasp --help' for usage."
printf "Reload bash so the new PATH is active? [yes/no]: "
read ans
case "$ans" in
  yes|YES|Yes) exec bash ;;
esac

