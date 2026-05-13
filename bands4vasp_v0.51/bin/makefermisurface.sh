#!/bin/sh
# -----------------------------------------------------------------------------
# Arguments:
# $1 = base path (project root, where venv and Python scripts are located)
# $2 = temporary directory (will erased after finished)
# $3 = enable visualization (1=yes, 0=no)
# $4 = archetype folder with VASP files
# $5 = number of equidistant sampling steps (>=2) (integer)
# $6 = (optional) name prefix for created folders
# -----------------------------------------------------------------------------

pacdir="$1"
tmpdir="$2"
previsual="$3"
folder="$4"
num="$5"
fname="$6"


# Temporary filenames
kcoord="$tmpdir/band4vasp_KPOINTScoordinates"
emptyk="$tmpdir/band4vasp_emptyKPOINTS"
tempsurffile="$tmpdir/surface_sampling_points.txt"

. "$pacdir/bin/INPAR_utils.sh"

# --- Basic input validation ---

if [ ! -f "$folder/KPOINTS" ]; then
  echo "ERROR: The directory you passed ('$folder') does not contain a KPOINTS file."
  exit 1
fi

# Look for the #makepath flag in the KPOINTS file
flag=$(grep -in '#makepath' "$folder/KPOINTS")
if [ -z "$flag" ]; then
  echo "ERROR: Couldn't find the '#makepath' flag in the KPOINTS file: $folder/KPOINTS"
  echo "       For more information see --info or the documentation."
  exit 1
fi

# Ensure $num is an integer >=2
while [ -z "$num" ] || [ "$num" -lt 2 ] || expr "$num" : '^[0-9][0-9]*$' = 0 >/dev/null 2>&1; do
  echo "Please enter the number of equidistant sampling steps (>=2):"
  read num
  echo ""
done


# Prompt for folder name prefix if not given
if [ -z "$fname" ]; then
  echo "Please enter the name prefix for the new folders."
  echo "Or press enter to use the default name 'surface':"
  read fname
  [ -z "$fname" ] && fname="surface"
fi

# --- Prepare to extract surface points from KPOINTS ---

nline=$(echo "$flag" | cut -d':' -f1)
i=$((nline - 1))
sed -n "1,${i}p" "$folder/KPOINTS" > "$emptyk"
i=$((nline + 1))
filelines=$(wc -l < "$folder/KPOINTS")
nci=0

rm -f "$kcoord"

# Read the three surface points, skipping empty and comment lines (even with leading spaces)
while [ "$i" -le "$filelines" ]; do
  tempcoord=$(sed -n "${i}p" "$folder/KPOINTS" | awk '{print $1" "$2" "$3}')
  # Skip empty or comment lines (even with leading spaces)
  if echo "$tempcoord" | grep -q '^[[:space:]]*#'; then
    i=$((i+1))
    continue
  fi
  if [ -z "$tempcoord" ]; then
    i=$((i+1))
    continue
  fi

  # Check that each of the three fields is a number
  set -- $tempcoord
  x1=$1; y1=$2; z1=$3
  expr "$x1" : '^-*[0-9]' >/dev/null 2>&1 || { echo "ERROR: The coordinate in line $i is not 3-dimensional!"; exit 1; }
  expr "$y1" : '^-*[0-9]' >/dev/null 2>&1 || { echo "ERROR: The coordinate in line $i is not 3-dimensional!"; exit 1; }
  expr "$z1" : '^-*[0-9]' >/dev/null 2>&1 || { echo "ERROR: The coordinate in line $i is not 3-dimensional!"; exit 1; }

  echo "$tempcoord" >> "$kcoord"
  nci=$((nci+1))
  i=$((i+1))
done

# Check that 3 points were found for the surface
if [ "$nci" -ne 3 ]; then
  echo "ERROR: There are not 3 coordinates to define the equidistant sampling."
  echo "       For more information enter 'b4vasp --info' or see the documentation."
  exit 1
fi

echo "Read 3 points defining the surface:"
cat "$kcoord"
echo ""

# --- Generate equidistant lines on the surface using Fortran tool ---
getsurfacelines4vasp "$num" "$kcoord" "$tempsurffile"

# --- For each line, prepare a VASP folder and its KPOINTS file ---

i=1
line=1
while [ "$i" -le "$num" ]; do
  newf="${i}${fname}"
  if [ -d "$newf" ]; then
    echo "WARNING: The directory '$newf' already exists."
    echo "         Do you want to replace it? [y/n]"
    read answ
    case "$answ" in
      [Nn]*) echo "Skipped '$newf'"; i=$((i+1)); continue ;;
      *) rm -rf "$newf" ;;
    esac
  fi

  cp -r "$folder" "$newf"
  cp -f "$emptyk" "$newf/KPOINTS"

  # Each sampling path consists of two points (one per line)
  for row in 0 1; do
    sed -n ${line}p "$tempsurffile" >> "$newf/KPOINTS"
    line=$((line+1))
  done

  # Optional: adapt job name for batch system scripts
  # jobname=$(grep 'JOB_ID=' "$newf/jobEBS" | cut -d"'" -f2)
  # sed -i "s/JOB_ID='$jobname'/JOB_ID='${jobname}${i}'/" "$newf/jobEBS"

  echo "Prepared $newf"
  i=$((i+1))
done

if check_value_string "$previsual";then
    . "$pacdir/venv/bin/activate"
    python3 "$pacdir/bin/visualize_sampling.py" "$tempsurffile"
    deactivate
fi

# --- Clean up temporary files ---
rm -f "$emptyk" "$kcoord" "$tempsurffile"
