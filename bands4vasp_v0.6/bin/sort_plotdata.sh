#!/bin/bash
# $1 = temporary directory
# $2 = allfilenames (list of all relevant file paths)
# $3 = fstat (status variable)
# $4 = lfermisurface (true/false: is this a fermi surface calculation)
# $5 = spin count (1 or 2)
# $6 = spin index (used if spin == 2)

tmpdir="$1"
alfnames="$2"
fstat="$3"
lfermisur="$4"
spin="$5"

# Handle spin suffix
if [ "$spin" -eq 2 ]; then
    spn=".spin$6"
else
    spn=''
fi

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
. "$SCRIPT_DIR/INPAR_utils.sh"

INPAR="$(sed -n 2p "$alfnames")"
plotdata="$(sed -n 3p "$alfnames")$spn.dat"
splotdata="$(sed -n 3p "$alfnames").slim$spn.dat"
fermifile="../$(sed -n 6p "$alfnames")$spn"
fsurfile="$(sed -n 20p "$alfnames")$spn"
fsurspecfile="$(sed -n 21p "$alfnames")$spn"
outpar="../$(sed -n 28p "$alfnames")" #needed for INPAR_utils

# --- Sort and generate files for plotdata ---
if [ -f "$plotdata" ]; then
    # Only proceed if file has numeric data lines
    if [ "$(sed '/#/d' "$plotdata" | grep -cE '[0-9]')" -gt 0 ]; then
        # Sort plotdata by the third column (Bloch character) and create new file
        sed '/^#/d' "$plotdata" | awk '{print $3" "$0 }' | sort -n -k1 | awk '{$1="";print $0}' > "bloch$plotdata"
        orbis="$(grep -E "#[ ,1-9][0-9]ORBITAL=*" "$plotdata" | cut -d'=' -f2)"
        norbis="$(echo "$orbis" | wc -l)"
    fi
fi

# --- Sort slim plotdata files ---
if [ -f "$splotdata" ]; then
    rm -f "bloch$splotdata" "orb$splotdata"
    if [ "$(sed '/#/d' "$splotdata" | grep -cE '[0-9]')" -gt 0 ]; then
        sed '/^#/d' "$splotdata" | sort -n -k3 > "bloch$splotdata"
        sed '/^#/d' "$splotdata" | sort -n -k4 > "orb$splotdata"
    fi
fi

# --- Handle Fermi file temp and sorting ---
fstat=$((2 * fstat))
if [ -f "$fermifile.temp" ]; then
    grep -E '^#' "$fermifile.temp" > "$fermifile.dat"
    sed '/^#/d' "$fermifile.temp" | sort -n -k1 >> "$fermifile.dat"
    rm -f "$fermifile.temp"
else
    fstat=$((fstat + 1))
fi

# --- Handle Fermi surface calculation data ---
# Test lfermisur as a boolean ("true"/"false" or 1/0)
if [ "$lfermisur" = "true" ] || [ "$lfermisur" = "1" ]; then
    if [ -f "$fsurfile.temp" ]; then
        grep -E '^#' "$fsurfile.temp" > "$fsurfile.dat"
        sed '/^#/d' "$fsurfile.temp" | sort -n -k3 >> "$fsurfile.dat"
        rm -f "$fsurfile.temp"
    fi
fi

# --- Spectral function data  ---
if get_inpar_value_by_name "SPECFUN"; then
    if [ -f "$fsurspecfile.temp" ]; then
        grep -E '^#' "$fsurspecfile.temp" > "$fsurspecfile.dat"
        sed '/^#/d' "$fsurspecfile.temp" | sort -n -k3 >> "$fsurspecfile.dat"
        rm -f "$fsurspecfile.temp"
    fi
fi

echo "$fstat"
