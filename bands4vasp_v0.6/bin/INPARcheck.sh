#!/bin/sh
# This script processes input parameters for bands4vasp
# $1 = INPAR file name
# $2 = default INPAR file with all default parameters
# $3 = temporary file directory
# $4 = OUTPAR file name


INPAR="$1"
oinpar="$2"
tmpdir="$3"
outpar="$4"

userinpar="$tmpdir/INPAR.user.temp"

# Liste der Parameter als String (leerzeichengetrennt)
parameter_list="EDELTA1 EDELTA2 EDIF BAVERAGE OAVERAGE EGAP BLOCH_THRESHOLD DBLOCH BNDDIFF KAPPANORM GRADIENTD NPOINTS LPOLY REGULAPREC PLOTORB ODISTINCT SYMREC SYMPOINT1 SYMPOINT2 BANDINDEXPLOT SIGMA SPECFUN SPECDELTA SLIMSPEC FGRID SELECTION SKIPKPOINT ROOTSCALC EFERMI MAKEPLOTS LEAVEPLOTDATA PSFAC LFITPOINTS LLINES LROOTS BCOLOURS OCOLOURS BACKCOLOUR FILEFORMAT PLOTSIZE FSUREDGEVEC FSURTICS PREPREVIEW PATHPOINTS"

# Clean user parameter input: remove comments and empty lines
if [ -f "$INPAR" ]; then
    sed 's/^[ \t]*//' "$INPAR" | sed '/^#/d;/^$/d' | cut -d'#' -f1 > "$userinpar"
fi

#--- Function: Check if keyword is in file and not commented
checkplotdefault() {
    dpfile="$1"
    dpword="$2"

    lword=$(grep -ic "$dpword" "$dpfile" 2>/dev/null)
    if [ "$lword" -eq 0 ]; then
        takefile="$oinpar"
        ldefaultvalue=true
    else
        firstchar=$(grep -i "$dpword" "$dpfile" | sed 's/^[ \t]*//' | cut -c1 | head -n1)
        case "$firstchar" in
            "#"|"!")
                takefile="$oinpar"
                ldefaultvalue=true
                ;;
            *)
                takefile="$dpfile"
                ldefaultvalue=false
                ;;
        esac
    fi

    firstchar=$(grep -i "$dpword" "$takefile" | sed 's/^[ \t]*//' | cut -c1 | head -n1)
    case "$firstchar" in
        "#"|"!")
            defaultvalue="#"
            ;;
        *)
            defaultvalue=$(grep -i "$dpword" "$takefile" | tail -n1 | cut -d'=' -f2- | sed 's/^[ \t]*//;s/[ \t]*$//')
            ;;
    esac
}

[ -f "$tmpdir/$INPAR" ] && rm -f "$tmpdir/$INPAR"

#--- Create temporary INPAR and output file
echo "$outpar::A list of all read $INPAR parameters" > "$outpar"
echo "--------------------------------------------" >> "$outpar"

for par_var in $parameter_list; do
    checkplotdefault "$userinpar" "$par_var"
    par="$defaultvalue"

    # EDELTA2 fallback
    if [ "$par_var" = "EDELTA2" ]; then
        checkplotdefault "$userinpar" "EDELTA1"
        ed1="$defaultvalue"
        if $ldefaultvalue && [ "$ed1" != "#" ]; then
            par="$ed1"
        fi
    fi

    echo "$par" >> "$tmpdir/$INPAR"
    echo "$par_var = $par" >> "$outpar"

    # EFERMI Flag (t/f)
    if [ "$par_var" = "EFERMI" ]; then
        echo "$par" | grep -iq "^bands$" && echo "t" >> "$tmpdir/$INPAR" || echo "f" >> "$tmpdir/$INPAR"
    fi
done

rm -f "$userinpar"


echo "A list of all read parameters was written in $outpar"
