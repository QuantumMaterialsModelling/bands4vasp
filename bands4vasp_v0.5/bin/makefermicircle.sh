#!/bin/sh
# -----------------------------------------------------------------------------
# Arguments:
# $1 = base path (project root, where venv and Python scripts are located)
# $2 = temporary directory (will erased after finished)
# $3 = enable visualization (1=yes, 0=no)
# $4 = archetype folder with VASP files
# $5 = number of equidistant sampling steps (>=2) (integer)
# $5 = (optional) name prefix for created folders
# -----------------------------------------------------------------------------

pacdir="$1"
tmpdir="$2"
previsual="$3"
folder="$4"
ntot="$5"
fname="$6"

# Temporary filenames
kcoord="$tmpdir/band4vasp_KPOINTScoordinates"
emptyk="$tmpdir/band4vasp_emptyKPOINTS"
circdat="$tmpdir/radial_fermisampling4VASP"
tempradfile="$tmpdir/radial_sampling_endpoints.txt"

. "$pacdir/bin/INPAR_utils.sh"


# --- Initial checks and input parsing ---

if [ ! -f "$folder/KPOINTS" ]; then
  echo "ERROR: The directory you passed '$folder' does not contain a KPOINTS file."
  exit 1
fi

flag=$(grep -in '#makepath' "$folder/KPOINTS")
if [ -z "$flag" ]; then
  echo "ERROR: Couldn't find the '#makepath' flag in the KPOINTS file: $folder/KPOINTS"
  echo "       See --info or the documentation for more details."
  exit 1
fi

# Ensure ntot is a valid positive integer
while ! echo "$ntot" | grep -Eq '^[0-9]+$'; do
  printf "Please enter the number of equidistant sampling steps: "
  read ntot
  echo ""
done

# Ask for folder name prefix if not given
if [ -z "$fname" ]; then
  printf "Please enter a prefix for the new folders\n"
  printf "Or you can press enter to use the default name 'radial': "
  read fname
  [ -z "$fname" ] && fname="radial"
fi

# --- Prepare coordinate extraction ---

nline=$(echo "$flag" | cut -d':' -f1)
i=$(($nline - 1))
sed -n "1,${i}p" "$folder/KPOINTS" > "$emptyk"
i=$(($nline + 1))

filelines=$(wc -l < "$folder/KPOINTS")
nci=0

rm -f "$kcoord"
while [ "$i" -le "$filelines" ]; do
  tempcoord=$(sed -n "${i}p" "$folder/KPOINTS" | awk '{print $1" "$2" "$3}')
  if [ -z "$tempcoord" ] || echo "$tempcoord" | grep -qE '^#'; then
    i=$(($i + 1))
    continue
  fi
  # Check that the current line has 3 valid coordinates
  j=1
  while [ "$j" -le 3 ]; do
    if ! echo "$tempcoord" | cut -d' ' -f"$j" | grep -Eq '^[-]?[0-9]'; then
      echo "ERROR: The coordinate in line $i is not 3-dimensional!"
      exit 1
    fi
    j=$(($j + 1))
  done
  echo "$tempcoord" >> "$kcoord"
  nci=$(($nci + 1))
  if [ "$nci" -eq 1 ]; then 
    startvec="$(sed -n ${i}p "$folder/KPOINTS")"
    echo "$startvec" >> "$emptyk"
  fi
  if [ "$nci" -eq 2 ]; then w1=$(sed -n "${i}p" "$folder/KPOINTS" | awk '{print $4}'); fi
  if [ "$nci" -eq 3 ]; then w2=$(sed -n "${i}p" "$folder/KPOINTS" | awk '{print $4}'); fi
  i=$(($i + 1))
done

if [ "$nci" -ne 3 ]; then
  echo "ERROR: There are not 3 coordinates to define the radial sampling."
  echo "       See --info or the documentation for more details."
  exit 1
fi


# --- Detect weight columns ---

nw=0
if echo "$w1" | grep -qE '[0-9]'; then
  nw=1
  if ! echo "$w2" | grep -qE '[0-9]'; then
    w2="$w1"
  fi
fi

# --- Run the Fortran radial sampling utility ---

getradlines4vasp "$ntot" "$kcoord" "$circdat" "$nw" "$w1" "$w2"

# --- For each generated sampling step, prepare a folder and its KPOINTS ---

i=1
while [ "$i" -le "$ntot" ]; do
  newf="${i}${fname}"
  if [ -d "$newf" ]; then
    echo "WARNING: The directory '$newf' already exists."
    printf "         Do you want to replace it? [y/n] "
    read answ
    if echo "$answ" | grep -qi '^n'; then
      echo "skipped '$newf'"
      i=$(($i + 1))
      continue
    else
      rm -r "$newf"
    fi
  fi
  cord2=$(sed -n "${i}p" "$circdat")
  cp -r "$folder" "$newf"
  cp "$emptyk" "$newf/KPOINTS"
  echo "$cord2" >> "$newf/KPOINTS"
  # Uncomment and adapt for per-job customization:
  # jobname=$(grep 'JOB_ID=' "$newf/jobEBS" | cut -d"'" -f2)
  # sed -i "s/JOB_ID='$jobname'/JOB_ID='${jobname}${i}'/" "$newf/jobEBS"
  echo "prepared $newf"
  i=$(($i + 1))
done

# --- Visualization if requested ---

if check_value_string "$previsual";then
    echo "$startvec" > "$tempradfile"
    cat "$circdat" >> "$tempradfile"
    # Activate the Python virtual environment for visualization
    . "$pacdir/venv/bin/activate"
    python3 "$pacdir/bin/visualize_sampling.py" "$tempradfile"
    deactivate
fi


# --- Cleanup temporary files ---

rm -f "$circdat" "$tempradfile" "$emptyk" "$kcoord"
