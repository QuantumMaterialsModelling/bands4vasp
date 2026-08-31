#!/bin/sh

# -----------------------------------------------------------------------------
# Arguments:
# $1 = base path (project root, where venv and Python scripts are located)
# $2 = temporary directory (will erased after finished)
# $3 = enable visualization (1=yes, 0=no)
# $4 = folder containing VASP input files (must contain KPOINTS)
# $5 = (optional) name prefix for new folders (will be asked if not provided)
# -----------------------------------------------------------------------------

pacdir="$1"
tmpdir="$2"
previsual="$3"
folder="$4"
fname="$5"

emptyk="$tmpdir/band4vasp_emptyKPOINTS"
templinesampling="$tmpdir/temp_line_sampling"

# --- Input validation ---

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

# Prompt for folder name prefix if not given
if [ -z "$fname" ]; then
  echo "Please enter the name prefix for the new folders."
  echo "Or press enter to use the default name 'lines':"
  read fname
  [ -z "$fname" ] && fname="lines"
fi

rm -f "$templinesampling"
nline=$(echo "$flag" | cut -d':' -f1)

# Extract header part before #makepath
i=$((nline - 1))
sed -n "1,${i}p" "$folder/KPOINTS" > "$emptyk"
i=$((nline + 1))
filelines=$(wc -l < "$folder/KPOINTS")

k=0
oldline=""

# --- Main loop: build line samples by point pairs ---

while [ "$i" -le "$filelines" ]; do
  newline=$(sed -n "${i}p" "$folder/KPOINTS")

  # Accept only lines that start with a digit or minus (coordinate lines)
  echo "$newline" | grep -Eq '^[[:space:]]*[-]?[0-9]' || {
    i=$((i + 1))
    continue
  }

  if [ "$k" -gt 0 ]; then
    newf="${k}${fname}"
    if [ -d "$newf" ]; then
      echo "WARNING: The directory '$newf' already exists."
      printf "         Do you want to replace it? [y/n] "
      read answ
      echo "$answ" | grep -qi '^n' && {
        echo "Skipped '$newf'"
        oldline="$newline"
        k=$((k + 1))
        i=$((i + 1))
        continue
      }
      rm -rf "$newf"
    fi

    # Prepare new folder with proper KPOINTS file for the line segment
    cp -r "$folder" "$newf"
    cp -f "$emptyk" "$newf/KPOINTS"
    echo "$oldline" >> "$newf/KPOINTS"
    echo "$newline" >> "$newf/KPOINTS"
    echo "$oldline" >> "$templinesampling"
    echo "$newline" >> "$templinesampling"

    # Uncomment and adapt for per-job customization:
    # jobname=$(grep 'JOB_ID=' "$newf/jobEBS" | cut -d"'" -f2)
    # sed -i "s/JOB_ID='$jobname'/JOB_ID='${jobname}${i}'/" "$newf/jobEBS"

    echo "*********** Prepared $newf **************"
    echo "Line: $oldline  >>  $newline"
    echo ""
  fi
  oldline="$newline"
  k=$((k + 1))
  i=$((i + 1))
done

# --- Preview the sampling using Python visualization (if enabled) ---

case "$previsual" in
  [.]T[Rr][Uu][Ee]. | [Tt][Rr][Uu][Ee] )
    . "$pacdir/venv/bin/activate"
    python3 "$pacdir/bin/visualize_sampling.py" "$templinesampling"
    deactivate
    ;;
esac


# --- Clean up temporary files ---
rm -f "$emptyk" "$templinesampling"

