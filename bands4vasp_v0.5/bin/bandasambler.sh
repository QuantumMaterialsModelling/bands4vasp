#!/bin/bash

# Arguments:
# $1: temporary directory (will be deleted at the end)
# $2: fermisurface or not
# $3: lfit
# $4: directory of bands4vasp
# $5: file with all filenames
# $6: file with all created plots
# $7: totspin
# $8: spin = {1,2}

tmpdir="$1"
lfsurface="$2"
lfit="$3"
packagedir="$4"
allfnames="$5"
created="$6"
totspin="$7"

# Robust test for spin and output file creation
if [ "$totspin" -eq 2 ]; then
  if [ "$8" -eq 1 ]; then
    rm -f "$created"
    echo -n "            ... creating gnuplot files ..."
  fi
  spin=".spin$8"
else
  spin=""
  rm -f "$created"
  echo -n "            ... creating gnuplot files ..."
fi


. "$packagedir/bin/INPAR_utils.sh"

# Read required filenames from $allfnames, always quote expansions!
inpar=$(sed -n 2p "$allfnames")
plotdata=$(sed -n 3p "$allfnames")
splotdata="$plotdata.slim$spin.dat"
plotdata="$plotdata$spin.dat"
specplotdata="$(sed -n 4p "$allfnames")$spin.dat"
outgrid=$(sed -n 5p "$allfnames")
fermifile="../$(sed -n 6p "$allfnames")$spin.dat"
fittpoints="$(sed -n 7p "$allfnames")$spin"
autocolordata="$(sed -n 17p "$allfnames")$spin"
fsurdat="$(sed -n 20p "$allfnames")$spin"
fsurgrid="$fsurdat.grid.dat"
fsurdat="$fsurdat.dat"
fsurspecdat="$(sed -n 21p "$allfnames")$spin.dat"
fsurpm3dspecdat="$(sed -n 21p "$allfnames")$spin.pm3d.dat"
outpar="../$(sed -n 28p "$allfnames")"

output="$(sed -n 10p "$allfnames")$spin.gnu"
tm1=$(sed -n 11p "$allfnames")
tm2=$(sed -n 12p "$allfnames")

imgdir="../$(sed -n 24p "$allfnames")"

# Directory selection depending on fermisurface flag
if $lfsurface; then
  bandstructure="$imgdir$(sed -n 13p "$allfnames")$spin"
  ebsbloch="$imgdir$(sed -n 14p "$allfnames")$spin"
else
  bandstructure="../$(sed -n 13p "$allfnames")$spin"
  ebsbloch="../$(sed -n 14p "$allfnames")$spin"
fi
ebsblochproces="$imgdir$(sed -n 14p "$allfnames")$spin.processed"
bandstructureproces="$imgdir$(sed -n 13p "$allfnames")$spin.processed"
ebsorb="$imgdir$(sed -n 15p "$allfnames")"
bandindexp="$imgdir$(sed -n 16p "$allfnames")$spin"
fsurbloch="../$(sed -n 18p "$allfnames")$spin"
fsurorb="$imgdir$(sed -n 19p "$allfnames")"
fsurpure="../$(sed -n 20p "$allfnames")$spin"
orbitalstats="$(sed -n 27p "$allfnames")$spin.dat"

mkdir -p "$imgdir"

# Define palettes as string variables for gnuplot
blochpalette="( 0 'white', 0.05 'beige' ,0.25 'greenyellow', 0.4 'web-green', 0.65 'steelblue', 0.9 'midnight-blue')"
orbitpalette="( 0 'gray', 0.1 'beige',0.2 'lemonchiffon', 0.35 'tan1',0.6 'orange-red' ,0.75 'dark-orange' , 0.9 'orangered4')"
ionpalette="( 0 'gray' ,0.09 'pink', 0.35 'dark-pink', 0.7 'dark-violet')"

# Set fonts
titlefont='Times-new-roman,22'
pfont='Times-new-roman,16'

# Parse plotting window from temp$inpar
ptemp=$(sed -n 1p "$tmpdir/$inpar")
pwind1=$(echo "$ptemp" | awk '{print $1}')
nbord=$(echo "$ptemp" | grep -Eo '[-]?[0-9]+[\.]?[0-9]*' | wc -l)
if [ "$nbord" -gt 1 ]; then
  pwind2=$(echo "$ptemp" | awk '{print $2}')
else
  pwind2="$pwind1"
  pwind1="-$pwind2"
fi

ptemp=$(sed -n 2p "$tmpdir/$inpar")
fwind1=$(echo "$ptemp" | awk '{print $1}')
nbord=$(echo "$ptemp" | grep -Eo '[-]?[0-9]+[\.]?[0-9]*' | wc -l)
if [ "$nbord" -gt 1 ]; then
  fwind2=$(echo "$ptemp" | awk '{print $2}')
else
  fwind2="$fwind1"
  fwind1="-$fwind2"
fi

psfac=$(sed -n 33p "$tmpdir/$inpar")

userbpalette=false
bcolour1=$(sed -n 37p "$tmpdir/$inpar" | awk '{print $1}')
if [ "$bcolour1" != '#' ]; then
  bcolour2=$(sed -n 37p "$tmpdir/$inpar" | awk '{print $2}')
  userbpalette=true
else
  bcolour2='blue'
fi

useropalette=false
ocolour1=$(sed -n 38p "$tmpdir/$inpar" | awk '{print $1}')
if [ "$ocolour1" != '#' ]; then
  ocolour2=$(sed -n 38p "$tmpdir/$inpar" | awk '{print $2}')
  useropalette=true
else
  ocolour2='red'
fi

background=$(sed -n 39p "$tmpdir/$inpar" | awk '{print $1}')

iend=$(grep '#KPOINTgrid' "$outgrid" | awk '{print $2}')

# Files initializations with robust boolean detection
llines=0
if sed -n 35p "$tmpdir/$inpar" | grep -qi 'default'; then
  llinesraw=0
  llinesmani=1
else
  if sed -n 35p "$tmpdir/$inpar" | grep -qi 'true'; then
    llines=1
  else
    llines=0
  fi
  llinesraw=$llines
  llinesmani=$llines
fi

lroots=0
if sed -n 36p "$tmpdir/$inpar" | grep -qi 'default'; then
  lrootsraw=0
  lrootsmani=1
else
  if sed -n 35p "$tmpdir/$inpar" | grep -qi 'true'; then
    lroots=1
  else
    lroots=0
  fi
  lrootsraw=$lroots
  lrootsmani=$lroots
fi

lpoints=0
if sed -n 34p "$tmpdir/$inpar" | grep -qi 'default'; then
  lpointsraw=0
  lpointsmani=1
else
  if sed -n 35p "$tmpdir/$inpar" | grep -qi 'true'; then
    lpoints=1
  else
    lpoints=0
  fi
  lpointsraw=$lpoints
  lpointsmani=$lpoints
fi


lspecpm3d=0
if sed -n 24p "$tmpdir/$inpar" | grep -qi 'true'; then
  lspecpm3d=1
fi

fgrid=$(sed -n 25p "$tmpdir/$inpar" | awk '{print $1}')


x0=$(grep "#1 " "$outgrid" | awk '{print $2}')
x1=$(grep "#$iend " "$outgrid" | awk '{print $2}')

selectorb=$(sed -n 15p "$tmpdir/$inpar")

cros0=$(tail -n1 "$tmpdir/$inpar")

lxrange=true
if [ "$x0" = "$x1" ]; then
  lxrange=false
fi

rm -f "$output"



##############################################################
################# figure out fileformat ######################
##############################################################

# Get plotsize from temp$inpar, ignore leading whitespace
inparplotsize=$(sed -n 41p "$tmpdir/$inpar" | sed 's/^[ \t]*//')

# If plotsize is empty or the line is commented out, do not set plotsize
if [ -z "$inparplotsize" ] || echo "$inparplotsize" | grep -Eq '^\s*#'; then
  plotsize=""
else
  plotsize="size $inparplotsize"
fi

# Read fileformat from temp$inpar (e.g., pdf, png, eps)
fileformat=$(sed -n 40p "$tmpdir/$inpar" | awk '{print $1}')

# ====================================================================
# ============================ pdf ===================================
if echo "$fileformat" | grep -qi '^pdf$'; then

  titlefont='Times-new-roman,14'
  standardfont='Times-new-roman,9'
  pfont='Times-new-roman,10'
  vfont='Times-new-roman,9'
  ticsfont='Times-new-roman,5'
  smalfont='Times-new-roman,4'

  if [ -z "$plotsize" ]; then
    terminal="set terminal pdf font '$standardfont'"
  else
    terminal="set terminal pdf $plotsize font '$standardfont'"
  fi
  prefix='pdf'

  ylabel="set ylabel 'E - E_F (eV)' font '$vfont'"
  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ]; then
    cbfblochoffset="$cbblochoffset offset graph -0.115,graph -0.44"
    cbblochoffset="$cbblochoffset offset graph -0.082,graph -0.44"
  fi
  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})'"
  if [ -z "$plotsize" ]; then
    cbfspeclabel="$cbspeclabel font '$pfont' offset graph -0.118,graph 0.01"
    pm3dspeclabel="$cbspeclabel font '$pfont' offset graph -0.126,graph 0.01"
    cbspeclabel="$cbspeclabel font '$titlefont' offset graph -0.08,graph -0.03"
  fi
  cborblabel="font 'Times-new-roman,15'"
  if [ -z "$plotsize" ]; then cborblabel="$cborblabel offset graph -0.084,graph -0.25"; fi

  cforblabel="font '$titlefont' offset graph -0.114,graph -0.13 rotate by 90"

  pstemp=$(echo "scale=3;0.4*$psfac" | bc)
  statfp=$(echo "scale=2; 0.5 * $psfac" | bc)
  fstatfp=$(echo "scale=2; 0.6 * $psfac" | bc)
  bstatfp=$(echo "scale=2; 0.8 * $psfac" | bc)

  morbpsfac='0.25'
  orbpsfac='0.9'
  mforbpsfac='0.06'
  forbpsfac='0.4'
# ==================================================================
# ======================= png + png cairo ==========================
elif echo "$fileformat" | grep -Eq -i '^pngcairo$|^png$'; then

  titlefont='Times-new-roman,31'
  standardfont='Times-new-roman,21'
  pfont='Times-new-roman,28'
  vfont='Times-new-roman,25'
  ticsfont='Times-new-roman,11'
  smalfont='Times-new-roman,10'

  # Prüfen ob pngcairo von gnuplot unterstützt wird
  if gnuplot -e "set terminal pngcairo" >/dev/null 2>&1; then
    ftermi='pngcairo'
  else
    echo "WARNING: pngcairo not available, falling back to png (limited font/symbol support)" >&2
    ftermi='png'
  fi

  # Terminalgröße setzen
  if [ -z "$plotsize" ]; then
    terminal="set terminal $ftermi size 1500,1125 enhanced font '$standardfont'"
  else
    terminal="set terminal $ftermi $plotsize enhanced font '$standardfont'"
  fi

  prefix='png'

  ylabel="set ylabel 'E - E_F (eV)' font '$pfont'"

  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ]; then
    cbfblochoffset="$cbblochoffset offset graph -0.12,graph -0.44"
    cbblochoffset="$cbblochoffset offset graph -0.097,graph -0.45"
  fi

  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})'"
  if [ -z "$plotsize" ]; then
    cbfspeclabel="$cbspeclabel font '$pfont' offset graph -0.113,graph 0.01"
    pm3dspeclabel="$cbspeclabel font '$pfont' offset graph -0.113,graph 0.01"
    cbspeclabel="$cbspeclabel font '$titlefont' offset graph -0.093,graph -0.04"
  fi

  cborblabel="font '$titlefont'"
  if [ -z "$plotsize" ]; then cborblabel="$cborblabel offset graph -0.094,graph -0.25"; fi

  cforblabel="font '$titlefont' offset graph -0.107,graph -0.13 rotate by 90"

  pstemp=$(echo "scale=3; 0.9 * $psfac" | bc)
  statfp=$(echo "scale=2; 1.7 * $psfac" | bc)
  fstatfp=$(echo "scale=2; 1.6 * $psfac" | bc)
  bstatfp=$(echo "scale=2; 3.0 * $psfac" | bc)

  morbpsfac='1.25'
  orbpsfac='4.0'
  mforbpsfac='0.8'
  forbpsfac='2.8'
# ===========================================================
# ========================== eps ============================
else
  titlefont='Times-new-roman,19'
  standardfont='Times-new-roman,14'
  pfont='Times-new-roman,16'
  vfont='Times-new-roman,14'
  ticsfont='Times-new-roman,7'
  smalfont='Times-new-roman,5'

  terminal="set terminal postscript eps $plotsize enhanced color font '$standardfont'"

  ylabel="set ylabel 'E - E_F (eV)' font '$vfont' offset 1.8,0"
  prefix='eps'
  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ]; then
    cbfblochoffset="$cbblochoffset offset graph -0.136,graph -0.44"
    cbblochoffset="$cbblochoffset offset graph -0.114,graph -0.45"
  fi
  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})'"
  if [ -z "$plotsize" ]; then
    cbfspeclabel="$cbspeclabel font '$pfont' offset graph -0.132,graph 0.01"
    pm3dspeclabel="$cbspeclabel font '$pfont' offset graph -0.14,graph 0.01"
    cbspeclabel="$cbspeclabel font '$titlefont' offset graph -0.11,graph -0.04"
  fi
  cborblabel="font '$titlefont'"
  if [ -z "$plotsize" ]; then cborblabel="$cborblabel offset graph -0.113,graph -0.25"; fi

  cforblabel="font '$titlefont' offset graph -0.136,graph -0.112 rotate by 90"

  pstemp=$(echo "scale=3;0.7*$psfac" | bc)
  statfp=$(echo "scale=2; 0.8 * $psfac" | bc)
  fstatfp=$(echo "scale=2; 0.7 * $psfac" | bc)
  bstatfp=$(echo "scale=2; 1.6 * $psfac" | bc)

  morbpsfac='0.8'
  orbpsfac='2.0'
  mforbpsfac='0.35'
  forbpsfac='1.3'
fi


#########################################
#######  Make Grid and Xtics  ###########
#########################################

# Take pathnames from INPAR
temppath=$(sed -n 45p "$tmpdir/$inpar")

# If pathnames are commented, show x values
if [ "$temppath" = "#" ]; then
  temppath=""
  xtics=""
  xlabel="set xlabel 'k-points distance' font '$pfont'"
else
  i=1
  while [ $i -le "$iend" ]; do
    pntemp=$(echo "$temppath" | awk '{print $1}')
    if echo "$pntemp" | grep -qi '^/'; then
      pntemp=$(echo "$pntemp" | sed 's/\///')
      pathname[$i]="{/Symbol $pntemp}"
    else
      pathname[$i]="$pntemp"
    fi
    temppath=$(echo "$temppath" | awk '{$1=""; print $0}')
    i=$((i+1))
  done

  # Setting distances and printing KPOINTS grid file
  istart=1
  for i in $(seq $istart $iend); do
    gridklength=$(grep "#$i " "$outgrid" | awk '{print $2}')
    if [ $i -eq $iend ]; then
      xtics="${xtics}, \"${pathname[$i]}\" $gridklength)"
    elif [ $i -eq $istart ]; then
      xtics="(\"${pathname[$i]}\" $gridklength"
    else
      xtics="${xtics}, \"${pathname[$i]}\" $gridklength"
    fi
  done
  xlabel=""
fi

ops=0.0
opfont="$pfont"
topfsize=$(echo "$pfont" | cut -d',' -f2 | awk '{print $0}')
opfsize="$topfsize"

################# Preparation ###################

################ Orbital plots ##################
if [ "$lfit" -lt 6 ]; then
  # grep the orbital name
  allorbitals=$(grep -E "#[ ,1-9][0-9]ALLORBITS=" "$plotdata" | cut -d'=' -f2)
  sigorbitals=$(grep -E "#[ ,1-9][0-9]ORBITAL=" "$orbitalstats" | cut -d'=' -f2)
  nsigorbitals=$(echo "$sigorbitals" | wc -l)
  nallorbitals=$(echo "$allorbitals" | wc -l)
  selorb=$((selectorb - 1))
  if [ "$selectorb" -gt 0 ] && [ "$selectorb" -lt 10 ]; then
    if [ "$nallorbitals" -ge "$selectorb" ]; then
      orbname=$(grep "# $selorb""ALLORBITS=" "$plotdata" | cut -d'=' -f2)
    else
      echo "WARNING: Selected orbital number $selectorb is not in $plotdata"
      orbname=$(grep "# 0ORBITAL=" "$orbitalstats" | cut -d'=' -f2)
      echo "        changed to orbital $orbname"
    fi
  elif [ "$selectorb" -gt 10 ]; then
    if [ "$nallorbitals" -ge "$selectorb" ]; then
      orbname=$(grep "#$selorb""AllORBITs=" "$plotdata" | cut -d'=' -f2)
    else
      echo "WARNING: Selected orbital number $selectorb is not in $plotdata"
      orbname=$(grep "# 0ORBITAL=" "$orbitalstats" | cut -d'=' -f2)
      echo "        changed to orbital $orbname"
    fi
  elif [ "$selectorb" -eq 0 ]; then
    orbname="$sigorbitals"
    orbpsfac="$morbpsfac"
    forbpsfac="$mforbpsfac"
  else
    orbname='tot'
    selectorb="$nallorbitals"
  fi

  maxorb=$(grep '#maxorb ' "$orbitalstats" | awk '{print $2}')
  norbis=$(echo "$orbname" | wc -l)
  if [ "$selectorb" -lt 0 ]; then selectorb="$norbis"; fi
  lemptyorbs=0
  if [ "$selectorb" -eq 0 ]; then
    lemptyorbs=$(grep -c "#emptyorbs " "$orbitalstats")
    if [ "$lemptyorbs" -eq 1 ]; then
      emptyorbs=$(grep "#emptyorbs " "$orbitalstats" | cut -d' ' -f2-)
    fi
    orblayout="layout $(grep '#LAYOUT=' "$orbitalstats" | cut -d'=' -f2) margins 0.05,0.92,0.09,0.95 spacing 0.11,0.08"
    ops=$(echo "scale=3; 0.7-(1.0/$norbis)" | bc)
    opfsize=$(echo "scale=0; $topfsize-(15*$ops)/1" | bc)
    opfont=$(echo "$pfont" | cut -d',' -f1)","$opfsize
    ebsorbit="${ebsorb}_ALL${spin}"
  else
    ebsorbit="${ebsorb}_$orbname$spin"
    orblayout=""
  fi
  fsurorbit="${fsurorb}_$orbname$spin"

  cborblabel="set cblabel '{/:Bold Orbitalcharacter $orbname}' $cborblabel"
  cforblabel="set cblabel '{/:Bold Interpolated orbitalcharacter $orbname}' $cforblabel"

  if [ -f "$fsurdat" ]; then
    lfemptyorbs=$(grep -c "#emptyorbs " "$fsurdat")
    fmaxorb=$(grep '#maxorb ' "$fsurdat" | awk '{print $2}')
    if [ "$lfemptyorbs" -eq 1 ]; then
      femptyorbs=$(grep "#emptyorbs " "$fsurdat" | cut -d' ' -f2-)
    fi
    forbis=$(grep -E "#[ ,1-9][0-9]ORBITAL=" "$fsurdat" | cut -d'=' -f2)
    fnorbis=$(echo "$forbis" | wc -l)
    if [ "$fnorbis" -gt 1 ]; then
      forblayout="layout $(grep '#LAYOUT=' "$fsurdat" | cut -d'=' -f2) margins 0.05,0.92,0.09,0.95 spacing 0.11,0.08"
      fops=$(echo "scale=3; 0.9-(1.0/$fnorbis)" | bc)
      fopfsize=$(echo "scale=0; $topfsize-(22*$fops)/1" | bc)
      fotpfsize=$(echo "scale=0; $topfsize-(8*$fops)/1" | bc)
      fotpfont=$(echo "$pfont" | cut -d',' -f1)","$fotpfsize
      fsurorbit="${fsurorb}_ALL${spin}"
    else
      fsurorbit="${fsurorb}_$orbname$spin"
      forblayout=""
      fops=0.0
      fopfsize=$topfsize
    fi
    if grep -q '#noorbital' "$fsurdat"; then
      florb=false
      fno='No'
      forbname=""
    else
      florb=true
      fno=''
    fi
  fi

  if grep -q '#noorbital' "$splotdata"; then
    slorb=false
    sno='No'
    sorbname=""
  else
    slorb=true
    sno=''
    sorbname="$orbname"
  fi

  if grep -q '#noorbital' "$orbitalstats"; then
    lorb=false
    no='No'
    orbname=""
  else
    lorb=true
    no=''
  fi
fi

# PROCAR file
if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
  orbitvarps1='using 1:2'
  forbitvarps1='using 1:2'
  orbitvarps1s='using 1:2'
  forbitvarps1s='using 4:(0)'
  fovarps1='using 4:(0)'
  orbitvarps2="with points pt 7 ps $(echo "scale=3; $psfac * ($statfp-$ops)" | bc)"
  fovarps2="with points pt 7 ps $(echo "scale=3; $psfac * (0.2+$statfp-$ops)" | bc)"
# PROCAR.prim or PRJCAR file
else

  orbitvarps1="using 1:2:((\$3*$orbpsfac)*$psfac)"
  forbitvarps1="using 1:2:((\$3*$forbpsfac)*$psfac)"
  #forbitvarps1="using 1:2:((\$3 - 0.1) * $psfac)"

  if echo "$ops" | grep -q '-'; then
    fovarps1="using 4:(0):(\$6+$(echo "scale=2; $psfac * ($statfp-0.3$ops)" | bc))"
  else
    fovarps1="using 4:(0):(\$6+$(echo "scale=2; $psfac * ($statfp-0.3-$ops)" | bc))"
  fi
  orbitvarps1s="using 1:2:(\$3+$(echo "scale=3; $psfac * ($statfp-0.1)" | bc))"
  if [ "$slorb" = true ]; then
    forbitvarps1s="using 4:(0):(\$6+0.4+($psfac *$statfp))"
  else
    forbitvarps1s="using 4:(0):(\$6+($psfac * ($statfp - 0.2)))"
  fi
  orbitvarps2='with points pt 7 ps var'
  fovarps2='with points pt 7 ps var'
fi

if [ "$lroots" -eq 1 ] && [ $((lfit%2)) -ne 0 ] && get_inpar_value_by_name 'ROOTSCALC' && [ "$cros0" -eq 1 ]; then
  kdist=$(sed -n 1p "$fittpoints.dat")
  kmid=$(sed -n 2p "$fittpoints.dat")
  egap1=$(sed -n 3p "$fittpoints.dat")
  egap2=$(sed -n 4p "$fittpoints.dat")
  egap=$(sed -n 5p "$fittpoints.dat")
  emid=$(sed -n 6p "$fittpoints.dat")
  segap1=$(sed -n 7p "$fittpoints.dat")
  segap2=$(sed -n 8p "$fittpoints.dat")
  segap=$(sed -n 9p "$fittpoints.dat")
  semid=$(sed -n 10p "$fittpoints.dat")
  fgap1="set object 2 rect from first 0.0,$egap2 to $kdist,$egap1"
  fgap2="set object 2 fc rgb 'red' fs solid 0.5 behind"
  fgap3="set label 2 at first $kmid,$emid '{/Symbol D}E = $egap eV' font '$pfont'"
  sfgap1="set object 3 rect from first 0.0,$segap2 to $kdist,$segap1"
  sfgap2="set object 3 fc rgb 'red' fs solid 0.5 behind"
  sfgap3="set label 3 at first $kmid,$semid '{/Symbol D}E = $segap eV' font '$pfont'"
else
  fgap1=""
  fgap2=""
  fgap3=""
  sfgap1=""
  sfgap2=""
  sfgap3=""
fi


###########################################################################
#================= Get all available Gnuplot colours ======================
###########################################################################
# Try to read color names from Gnuplot; fallback to package table if not available
gnuplot -e 'show palette colornames' > color_table.temp 2>&1

colnums=$(grep -E 'There are [0-9]+ predefined color names:' color_table.temp | grep -oE '[0-9]+')
if [ -n "$colnums" ]; then
  n=1
  nshift=$(grep -nE 'There are [0-9]+ predefined color names:' color_table.temp | cut -d':' -f1)
  while [ "$n" -le "$colnums" ]; do
    line=$((n + nshift))
    color_table[$n]=$(sed -n "${line}p" color_table.temp | awk '{print $1}')
    n=$((n+1))
  done
else
  n=1
  colnums=$(wc -l < "$packagedir/gnuplot_color_table")
  while [ "$n" -le "$colnums" ]; do
    color_table[$n]=$(sed -n "${n}p" "$packagedir/gnuplot_color_table")
    n=$((n+1))
  done
fi
rm -f color_table.temp
nctable=${#color_table[@]}

####################################################################
#******************************************************************#
################## Creating gnuplot plotter file ###################
#******************************************************************#
####################################################################

#----------------------------------------------------------------
#==================== EBSbloch / Bandstructure ==================
#----------------------------------------------------------------

echo "$terminal" > "$output"
if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
  echo "set output '$bandstructure.$prefix'" >> "$output"
  echo "$bandstructure.$prefix" >> "$created"
else
  echo "set output '$ebsbloch.$prefix'" >> "$output"
  echo "$ebsbloch.$prefix" >> "$created"
  if [ "$userbpalette" = true ]; then
    echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> "$output"
  else
    echo "set palette defined $blochpalette" >> "$output"
  fi
fi

echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> "$output"
echo "set object 1 fc rgb '$background' fs solid 1.0 behind" >> "$output"
if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi
cat >> "$output" <<catOUT
set yrange [$pwind1:$pwind2]
unset key
$xlabel
$ylabel
set format "%4.2g"
set cbrange [0.0:1.0]
set cbtics font "$vfont"
$cbblochoffset
set multiplot
set xtics $xtics font "$vfont" offset 0,0.4
set ytics font "$vfont" offset 0.2,0
$fgap1
$fgap2
$fgap3
p [][]\\
catOUT

if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
  if grep -Ecv '^#' "$plotdata" | grep -q '[0-9]'; then
    echo " '$plotdata' u 1:2 w p pt 7 ps $statfp lc rgb '$bcolour2',\\" >> "$output"
  fi
else
  if grep -Ecv '^#' "$plotdata" | grep -q '[0-9]'; then
    echo " 'bloch$plotdata' u 1:2:3 w p pt 7 ps $statfp palette,\\" >> "$output"
  fi
fi
echo -n "'$outgrid' w l ls 0, 0 ls 0" >> "$output"
if [ $((lfit % 2)) -eq 0 ]; then
  if [ "$llinesraw" -eq 1 ]; then
    echo ',\' >> "$output"
    echo -n "'$fittpoints.lines.dat' u 1:2  w l lt 2 lw $bstatfp lc rgb 'orange'" >> "$output"
  fi
  if [ "$lpointsraw" -eq 1 ]; then
    echo ',\' >> "$output"
    echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $fstatfp lc rgb 'orange'" >> "$output"
  fi
  if [ "$lrootsraw" -eq 1 ]; then
    echo ',\' >> "$output"
    echo -n "'$fermifile' u 4:(0) w p pt 7 ps $statfp lc rgb 'dark-orange'" >> "$output"
  fi
fi
echo "" >> "$output"

# =================== Spectral function plot =======================
if ! $lfsurface && get_inpar_value_by_name 'SPECFUN' && grep -Ev '^#' "$specplotdata" | grep -q '[0-9]'; then
  cbmax=$(sort -k4 "$specplotdata" | tail -n1 | awk '{print $4}')

  cat >> "$output" <<catOUT
unset multiplot
reset
unset key

$terminal
catOUT

  if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
    echo "set out '$bandstructure.spec.$prefix'" >> "$output"
    echo "$bandstructure.spec.$prefix" >> "$created"
  else
    echo "set out '$ebsbloch.spec.$prefix'" >> "$output"
    echo "$ebsbloch.spec.$prefix" >> "$created"
  fi

  echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> "$output"
  echo "set object 1 fc rgb 'black' fs solid 1.0 behind" >> "$output"
  if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi
  cat >> "$output" <<catOUT
set yrange [$pwind1:$pwind2]
set multiplot
$xlabel
$ylabel
set format "%4.2g"
set cbtics font "$vfont"
$cbspeclabel
set cbrange [0.0:$cbmax]
set xtics $xtics font "$vfont" offset 0,0.4
set ytics font "$vfont" offset 0.2,0
p [][]\\
 '<sort -k4 $specplotdata' u 2:3:4 w p pt 7 ps $pstemp palette, \\
 '$outgrid' w l ls 0 lc rgb 'white', 0 ls 0 lc rgb 'white'

catOUT
fi

# =================== Processed Bandstructure Plot =======================
if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
  procbandfile="$splotdata"
else
  procbandfile="bloch$splotdata"
fi

if [ -f "$procbandfile" ]; then
  if grep -Ev '^#' "$procbandfile" | grep -q '[0-9]'; then
    cat >> "$output" <<catOUT
unset multiplot
reset
unset key

$terminal
catOUT

    if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
      echo "set out '$bandstructureproces.$prefix'" >> "$output"
      echo "$bandstructureproces.$prefix" >> "$created"
    else
      echo "set out '$ebsblochproces.$prefix'" >> "$output"
      echo "$ebsblochproces.$prefix" >> "$created"
      if [ "$userbpalette" = true ]; then
        echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> "$output"
      else
        echo "set palette defined $blochpalette" >> "$output"
      fi
    fi

    echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> "$output"
    echo "set object 1 fc rgb '$background' fs solid 1.0 behind" >> "$output"
    if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi
    cat >> "$output" <<catOUT
set yrange [$fwind1:$fwind2]
$xlabel
$ylabel
set format "%4.2g"
set cbrange [0.0:1.0]
set cbtics font "$vfont"
$cbblochoffset
set multiplot
set xtics $xtics font "$vfont" offset 0,0.4
set ytics font "$vfont" offset 0.2,0
$sfgap1
$sfgap2
$sfgap3
p [][]\\
catOUT

    if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
      if grep -Ev '^#' "$splotdata" | grep -q '[0-9]'; then
        echo " '$splotdata' u 1:2 w p pt 7 ps $statfp lc rgb '$bcolour2',\\" >> "$output"
      fi
    else
      if grep -Ev '^#' "bloch$splotdata" | grep -q '[0-9]'; then
        echo " 'bloch$splotdata' u 1:2:3 w p pt 7 ps $statfp palette,\\" >> "$output"
      fi
    fi
    echo -n "'$outgrid' w l ls 0, 0 ls 0" >> "$output"
    if [ $((lfit % 2)) -eq 0 ]; then
      if [ "$llinesmani" -eq 1 ]; then
        echo ',\' >> "$output"
        echo -n "'$fittpoints.lines.dat' u 1:2  w l lt 2 lw $bstatfp lc rgb 'orange'" >> "$output"
      fi
      if [ "$lpointsmani" -eq 1 ]; then
        echo ',\' >> "$output"
        echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $bstatfp lc rgb 'orange'" >> "$output"
        if [ "$lfit" -lt 4 ] || [ "$lfit" -gt 5 ]; then
          echo ',\' >> "$output"
          echo -n "'$fittpoints.dat' u 1:2:3 w p pt 7 ps $statfp palette" >> "$output"
        fi
      fi
      if [ "$lrootsmani" -eq 1 ]; then
        echo ',\' >> "$output"
        echo -n "'$fermifile' u 4:(0) w p pt 7 ps $bstatfp lc rgb 'dark-orange'" >> "$output"
        if [ "$lfit" -lt 4 ] || [ "$lfit" -gt 5 ]; then
          echo ',\' >> "$output"
          echo -n "'$fermifile' u 4:(0):6 w p pt 7 ps $statfp palette" >> "$output"
        fi
      fi
    fi
    echo "" >> "$output"
  fi
fi




#################################################################
#----------------------------------------------------------------
#============================= EBSorbit =========================
#----------------------------------------------------------------


if [ $lfit -lt 6 ]&&[ `sed '/#/d' $plotdata |grep -cE '[0-9]'` -gt 0 ];then
# Set defaults if empty or not numeric
norbis=${norbis:-0}; [[ "$norbis" =~ ^[0-9]+$ ]] || norbis=0
lemptyorbs=${lemptyorbs:-0}; [[ "$lemptyorbs" =~ ^[0-9]+$ ]] || lemptyorbs=0
selectorb=${selectorb:-0}; [[ "$selectorb" =~ ^[0-9]+$ ]] || selectorb=0
opfsize=${opfsize:-12}; [[ "$opfsize" =~ ^[0-9]+$ ]] || opfsize=12


echo "$ebsorbit.$prefix" >> "$created"
cat >> "$output" <<catOUT
unset multiplot
reset

$terminal
set output '$ebsorbit.$prefix'

unset key
set multiplot $orblayout

set format "%4.2g"
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
set cbrange [0:$maxorb]
set yrange [$pwind1:$pwind2]
catOUT
if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi

if [ "$useropalette" = true ]; then
  echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> "$output"
else
  echo "set palette defined $orbitpalette" >> "$output"
fi

if [ "$lorb" = true ]; then
  if [ "$norbis" -eq 1 ]; then
    cat >> "$output" <<catOUT
set xtics $xtics font "$vfont" offset 0, 0.5
set ytics font "$vfont" offset 0.2,0    
$xlabel
$ylabel
$cborblabel
set cbtics font "$vfont"
$fgap1
$fgap2
$fgap3
catOUT
else
cat >> "$output" <<catOUT
set xtics $xtics font "$smalfont" offset 0, 0.5
set ytics font "$smalfont" offset 0.2, 0
set cbtics font "$smalfont" offset -0.3,0
catOUT
  fi
fi

if [ "$norbis" -gt 1 ] && [ "$lemptyorbs" -eq 1 ] && [ "$selectorb" -eq 0 ]; then
  echo "set label 2 at screen 0.02,0.02 '{/:Bold *no orbital character above 0.01 for orbitals: $emptyorbs}' font 'times-new-roman, $((opfsize + 4))'" >> "$output"
fi

or=0
while [ "$or" -lt "$norbis" ]; do

  if [ "$selectorb" -gt 0 ] && [ "$nallorbitals" -ge "$selectorb" ]; then
    or=$((selectorb - 1))
    if [ "$or" -lt 10 ]; then
      orbitaltitel=$(grep "# $or""ALLORBITS=" "$plotdata" | cut -d'=' -f2)
    else
      orbitaltitel=$(grep "#$or""ALLORBITS=" "$plotdata" | cut -d'=' -f2)
    fi
    orow="$or"
  else
    if [ "$or" -lt 10 ]; then
      orbitaltitel=$(grep "# $or""ORBITAL=" "$orbitalstats" | cut -d'=' -f2)
    else
      orbitaltitel=$(grep "#$or""ORBITAL=" "$orbitalstats" | cut -d'=' -f2)
    fi
    orow=$(grep -nE "#[ ,1-9][0-9]ALLORBITS=*" "$plotdata" | grep -E "$orbitaltitel$" | cut -d':' -f1)
    orow=$((orow - 1))
  fi

  #echo "set label 1 at graph 0.45,1.09 '{/:Bold $orbitaltitel}' font 'times-new-roman, $((opfsize + 3))'" >> "$output"
  echo "set label 1 at graph 0.45,1.09 '{/:Bold $orbitaltitel}' font '$pfont'" >> "$output"
  echo 'p [][]\' >> "$output"
  if [ "$lorb" = true ]; then
    if sed '/#/d' "$plotdata" | grep -qE '[0-9]'; then
      echo "'<sed \"/^#/d\" $plotdata | sort -n -k$((4 + orow))' $orbitvarps1:$((4 + orow)) $orbitvarps2 palette, \\" >> "$output"
    fi
  else
    if sed '/#/d' "$plotdata" | grep -qE '[0-9]'; then
      echo "'<sed \"/^#/d\" $plotdata | sort -n -k$((4 + orow))' $orbitvarps1 $orbitvarps2 lc rgb '$ocolour2', \\" >> "$output"
    fi
  fi

  echo -n "'$outgrid' w l ls 0, 0 ls 0" >> "$output"
  if [ $((lfit % 2)) -eq 0 ] && [ "$norbis" -eq 1 ]; then
    if [ "$llinesraw" -eq 1 ]; then
      echo ',\' >> "$output"
      echo -n "'$fittpoints.lines.dat' u 1:2 w l lt 2 lw $bstatfp lc rgb 'green'" >> "$output"
    fi
    if [ "$lpointsraw" -eq 1 ]; then
      echo ',\' >> "$output"
      echo -n "'$fittpoints.dat' $orbitvarps1 $orbitvarps2 lc rgb 'green'" >> "$output"
    fi
    if [ "$lrootsraw" -eq 1 ]; then
      echo ',\' >> "$output"
      echo -n "'$fermifile' u 4:(0) w p pt 7 ps 1.0 lc rgb 'blue'" >> "$output"
    fi
  fi
  echo "" >> "$output"
  or=$((or + 1))
done

#####################################################################
#--------------------------------------------------------------------
#======================= EBSorbit.processed =========================
#--------------------------------------------------------------------

if [ "$norbis" -eq 1 ]; then
  if [ -f "orb$splotdata" ]; then
    if sed '/#/d' "orb$splotdata" | grep -qE '[0-9]'; then
      echo "$ebsorbit.processed.$prefix" >> "$created"
      cat >> "$output" <<catOUT
unset multiplot
reset

$terminal
set output '$ebsorbit.processed.$prefix'
unset key
$xlabel
$ylabel
set multiplot
set ytics font "$vfont" offset 0.1,0
set xtics $xtics font "$vfont" offset 0,0.5
set yrange [$fwind1:$fwind2]
catOUT
      if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi

      echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> "$output"
      echo "set object 1 fc rgb '$background' fs solid 0.5 behind" >> "$output"

      if [ "$slorb" = true ]; then
        if [ "$useropalette" = true ]; then
          echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> "$output"
        else
          echo "set palette defined $orbitpalette" >> "$output"
        fi
        cat >> "$output" <<catOUT
set format "%4.2g"
set cbtics font "$vfont"
$cborblabel
$sfgap1
$sfgap2
$sfgap3
catOUT
      fi
      echo "plot [][]\\" >> "$output"
      if [ "$slorb" = true ]; then
        if sed '/#/d' "orb$splotdata" | grep -qE '[0-9]'; then
          echo "'orb$splotdata' $orbitvarps1s:4 $orbitvarps2 palette, \\" >> "$output"
        fi
      else
        if sed '/#/d' "orb$splotdata" | grep -qE '[0-9]'; then
          echo "'orb$splotdata' $orbitvarps1s $orbitvarps2 lc rgb '$ocolour2', \\" >> "$output"
        fi
      fi
      echo -n "'$outgrid' w l ls 0, 0 ls 0" >> "$output"
      if [ $((lfit % 2)) -eq 0 ]; then
        if [ "$llinesmani" -eq 1 ]; then
          echo ',\' >> "$output"
          echo -n "'$fittpoints.lines.dat' u 1:2 w l lt 2 lw $bstatfp lc rgb 'green'" >> "$output"
        fi
        if [ "$lpointsmani" -eq 1 ]; then
          if [ "$lfit" -lt 4 ] || [ "$lfit" -gt 5 ]; then
            echo ',\' >> "$output"
            echo -n "'$fittpoints.dat' u 1:2:(\$3+0.2+$psfac*$statfp) w p pt 7 ps var lc rgb 'green'" >> "$output"
            if [ "$slorb" = true ]; then
              echo ',\' >> "$output"
              echo -n "'$fittpoints.dat' u 1:2:(\$3+$psfac*($statfp-0.2)):4 w p pt 7 ps var palette" >> "$output"
            fi
          else
            echo ',\' >> "$output"
            echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $statfp lc rgb 'green'" >> "$output"
            if [ "$slorb" = true ]; then
              echo ',\' >> "$output"
              echo -n "'$fittpoints.dat' u 1:2:4 w p pt 7 ps $fstatfp palette" >> "$output"
            fi
          fi
        fi
        if [ "$lrootsmani" -eq 1 ]; then
          if sed '/#/d' "$fermifile" | grep -qE '[0-9]'; then
            echo ',\' >> "$output"
            echo -n "'$fermifile' $forbitvarps1s $fovarps2 lc rgb 'blue'" >> "$output"
            if [ "$slorb" = true ]; then
              echo ',\' >> "$output"
              echo -n "'$fermifile' $fovarps1:7 $orbitvarps2 palette" >> "$output"
            fi
          fi
        fi
      fi
      echo "" >> "$output"
    fi
  fi
 fi
fi # norbis -eq 1

#########################################################################
#=======================================================================#
#============================ Bandindexplot ============================#
#=======================================================================#
#########################################################################
if get_inpar_value_by_name 'BANDINDEXPLOT' && sed '/#/d' "$plotdata" | grep -qE '[0-9]'; then

  echo "$bandindexp.$prefix" >> "$created"
  cat >> "$output" <<catOUT
unset multiplot
reset

$terminal
set output '$bandindexp.$prefix'
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
catOUT
  if [ "$lxrange" = true ]; then echo "set xrange [$x0:$x1]" >> "$output"; fi
  cat >> "$output" <<catOUT
set yrange [$pwind1:$pwind2]
unset key
$xlabel
$ylabel
set format "%4.2g"
set multiplot
set xtics $xtics font "$vfont" offset 0,0.4
set ytics font "$pfont" offset 0.2,0
set key title 'Bandindex'
set key outside right top box 3
$fgap1
$fgap2
$fgap3
p [][]\\
catOUT

  cn=1
  for bpf in bandindexplot/*; do

    # Filter for spin channel, if needed
    if [ "$totspin" -eq 2 ]; then
      if ! echo "$bpf" | grep -q "$spin"; then continue; fi
    fi

    # Cycle through color table, skip if matches background color
    if [ "$cn" -ge "$nctable" ]; then cn=1; fi
    if echo "$background" | grep -Eiq "^${color_table[$cn]}$"; then
      cn=$((cn + 1))
    fi
    if [ "$cn" -ge "$nctable" ]; then cn=1; fi

    if sed '/#/d' "$bpf" | grep -qE '[0-9]'; then
      bpftitle=$(basename "$bpf" | grep -Eo '[0-9]{1,4}\.dat$' | grep -Eo '[0-9]{1,4}')
      if [ "$lfit" -eq 4 ] || [ "$lfit" -eq 5 ]; then
        echo " '$bpf' u 1:2 title '$bpftitle' w lines lw 2 lc rgb '${color_table[$cn]}',\\" >> "$output"
      else
        echo " '$bpf' u 1:2:(\$3+$statfp-0.5) title '$bpftitle' w p pt 7 ps var lc rgb '${color_table[$cn]}',\\" >> "$output"
      fi
    fi
    cn=$((cn + 1))
  done
  echo "'$outgrid' notitle w l ls 0, 0 notitle ls 0" >> "$output"
  echo "" >> "$output"
fi


#################################################################
#----------------------------------------------------------------
#========================= Fermisurface =========================
#----------------------------------------------------------------

# Only execute this block if Fermi surface plotting is requested
if $lfsurface; then
  isfsurdat=0
  # Plot string for standard or Bloch plotting, depending on $lfit
  psvar="1:2:4 w p pt 7 ps 0.6*$psfac"
  # Coordinates for the vectors at the edge of the fermi surface
  edgex1='0.05'
  edgex2='0.66'
  edgey2='0.96'

  if get_inpar_value_by_name 'FSURTICS';then
    edgey1='0.03' #0.05
  else
    edgey1='0.05' #0.05
  fi

  kedgecoord="$(grep '#KVEC ' "$fsurdat" | cut -d' ' -f2-)"
  kedge1="($(grep '#KEDGE1 ' "$fsurdat" | cut -d' ' -f2-))_{$kedgecoord}"
  kedge2="($(grep '#KEDGE2 ' "$fsurdat" | cut -d' ' -f2-))_{$kedgecoord}"
  kedge3="($(grep '#KEDGE3 ' "$fsurdat" | cut -d' ' -f2-))_{$kedgecoord}"
  kedge4="($(grep '#KEDGE4 ' "$fsurdat" | cut -d' ' -f2-))_{$kedgecoord}"


  cat >> "$output" <<catend
reset
unset key
unset multiplot
catend

  if [ -f "$fsurdat" ] && get_inpar_value_by_name 'ROOTSCALC' && [ $((lfit % 2)) -eq 0 ]; then
    if sed '/#/d' "$fsurdat" | grep -qE '[0-9]'; then

      isfsurdat=1
      fxb1=$(grep '#x ' "$fsurdat" | awk '{print $2}')
      fxb2=$(grep '#x ' "$fsurdat" | awk '{print $3}')
      fyb1=$(grep '#y ' "$fsurdat" | awk '{print $2}')
      fyb2=$(grep '#y ' "$fsurdat" | awk '{print $3}')

      if [ "$lfit" -eq 4 ]; then
        echo "$fsurpure.$prefix" >> "$created"
      else
        echo "$fsurbloch.$prefix" >> "$created"
        # PROCAR.prim or PRJCAR file (Blochcharacter)
        #psvar="1:2:($psfac*$statfp*\$3+0.5):4 w p pt 7 ps var"
        psvar="$forbitvarps1:4 w p pt 7 ps var"
      fi

      cat >> "$output" <<catend

$terminal
catend

if [ "$lfit" -eq 4 ]; then
    echo "set out '$fsurpure.$prefix'" >> "$output"
else
        cat >> "$output" <<catend
set output '$fsurbloch.$prefix'
$cbfblochoffset
set cbrange [0.0:1.0]
catend
fi

      cat >> "$output" <<catend
set size ratio 1
set xrange [$fxb1:$fxb2]
set yrange [$fyb1:$fyb2]
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb '$background' fillstyle solid 1.0 behind
set tmargin 3        
set bmargin 3.3
set lmargin 1
set rmargin 0
catend

if ! get_inpar_value_by_name 'FSURTICS'; then
  echo "unset xtics" >> "$output"
  echo "unset ytics" >> "$output"
else
  echo "set xtics offset 0, 0.5" >> "$output"
  echo "set ytics offset 0.2, 0" >> "$output"
fi

if [ "$kedgecoord" != 'n' ]; then
        cat >> "$output" <<catend
set label 1 at screen $edgex1,$edgey1 '$kedge1' font '$vfont'
set label 2 at screen $edgex2,$edgey1 '$kedge2' font '$vfont'
set label 3 at screen $edgex2,$edgey2 '$kedge3' font '$vfont'
set label 4 at screen $edgex1,$edgey2 '$kedge4' font '$vfont'
catend
fi

      # Bloch palette for non-lfit4 plots
      if [ "$lfit" -eq 4 ]; then
        echo -n "plot '$fsurdat' u 1:2 w p pt 7 ps $statfp*$psfac lc rgb 'blue'" >> "$output"
      else
        if [ "$userbpalette" = true ]; then
          echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> "$output"
        else
          echo "set palette defined $blochpalette" >> "$output"
        fi
        echo -n "plot '$fsurdat' u 1:2:3 w p pt 7 ps $statfp*$psfac palette" >> "$output"
      fi

      # Optionally plot grid
      if [ "$fgrid" -gt 0 ]; then
        echo ',\' >> "$output"
        echo "'$fsurgrid' notitle w l ls 0 lc rgb 'dark-gray'" >> "$output"
      fi

      echo "" >> "$output"
      echo "unset multiplot" >> "$output"

    fi
  fi
fi

#=============================================================================
#========================== Spectral Fermisurface ============================
#=============================================================================
if get_inpar_value_by_name 'SPECFUN'; then
  if { [ -f "$fsurspecdat" ] && [ "$lspecpm3d" -eq 1 ]; } || { [ "$lspecpm3d" -eq 0 ] && [ -f "$fsurpm3dspecdat" ]; }; then

    if sed '/#/d' "$fsurspecdat" | grep -qE '[0-9]'; then

      if [ "$lfit" -eq 4 ]; then
        specplotname="$fsurpure.spec.$prefix"
      else
        specplotname="$fsurbloch.spec.$prefix"
      fi

      echo "$specplotname" >> "$created"

      cat >> "$output" <<catend

$terminal
set output '$specplotname'
set size ratio 1
set palette
set cbrange [0.0:1.0]
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb "black" fillstyle solid 1.0
catend

      if [ $isfsurdat -eq 1 ];then
        echo "set xrange [$fxb1:$fxb2]" >> "$output"
        echo "set yrange [$fyb1:$fyb2]" >> "$output"
      fi

      if ! get_inpar_value_by_name 'FSURTICS'; then
        echo "unset xtics" >> "$output"
        echo "unset ytics" >> "$output"
      else
        echo "set xtics offset 0, 0.5" >> "$output"
        echo "set ytics offset 0.2, 0" >> "$output"
      fi


        if [ "$kedgecoord" != 'n' ]; then
          cat >> "$output" <<catend
set label 1 at screen $edgex1,$edgey1 '$kedge1' font '$vfont'
set label 2 at screen $edgex2,$edgey1 '$kedge2' font '$vfont'
set label 3 at screen $edgex2,$edgey2 '$kedge3' font '$vfont'
set label 4 at screen $edgex1,$edgey2 '$kedge4' font '$vfont'
catend
        fi

      if [ "$lspecpm3d" -eq 0 ]; then

        cat >> "$output" <<catend
set tmargin 0
set bmargin 0.5
set lmargin 0
set rmargin 0
$pm3dspeclabel
set pm3d map
set pm3d interpolate 2,2
set samples 1000
set isosamples 1000
catend

        echo -n "splot '$fsurpm3dspecdat' u 1:2:3 w pm3d" >> "$output"

      else
        cat >> "$output" <<catend
set tmargin 3
set bmargin 3
$cbfspeclabel
catend

        echo -n "plot '$fsurspecdat' u 1:2:3 w p pt 7 ps ($statfp-0.4)*$psfac palette" >> "$output"
      fi

      if [ "$fgrid" -gt 0 ]; then
        echo ',\' >> "$output"
        echo "'$fsurgrid' u 1:2:(0) w l notitle ls 0 lc rgb 'white'" >> "$output"
      else
        echo " " >> "$output"
      fi

    fi
  fi
fi

#======================================================================================
#=============================== Orbital Fermisurface =================================
#======================================================================================

# PROCAR[.prim] file (orbitalcharacter)
if [ "$lfit" -lt 6 ]; then
  if [ -f "$fsurdat" ] && get_inpar_value_by_name 'ROOTSCALC' && [ $((lfit % 2)) -eq 0 ]; then
    if sed '/#/d' "$fsurdat" | grep -qE '[0-9]'; then
      echo "$fsurorbit.$prefix" >> "$created"
      cat >> "$output" <<catend
reset
unset key

set output '$fsurorbit.$prefix'
set multiplot $forblayout
set size ratio -1
set xrange [$fxb1:$fxb2]
set yrange [$fyb1:$fyb2]
set format "%4.2g"
set cbrange [0.0:$fmaxorb]
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
catend
      if [ "$useropalette" = true ]; then
        echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> "$output"
      else
        echo "set palette defined $orbitpalette" >> "$output"
      fi

      if ! get_inpar_value_by_name 'FSURTICS'; then
        echo "unset xtics" >> "$output"
        echo "unset ytics" >> "$output"

      else
        echo "set xtics offset 0, 0.5" >> "$output"
        echo "set ytics offset 0.1, 0" >> "$output"

        if [ "$fnorbis" -eq 1 ];then
          echo "set tics font '$vfont'" >> "$output"
        else
          echo "set tics font '$ticsfont'" >> "$output"
        fi
      fi

      if [ "$fnorbis" -eq 1 ];then
        echo "set cbtics font '$vfont'" >> "$output"
      else
        echo "set cbtics font '$ticsfont'" >> "$output"
      fi

      if [ "$florb" = true ] && [ "$fnorbis" -eq 1 ]; then
        if [ "$kedgecoord" != 'n' ]; then

          cat >> "$output" <<catend
set tmargin 3
set bmargin 3
set lmargin 1
set rmargin 0     
set label 1 at screen $edgex1,$edgey1 '$kedge1' font '$vfont'
set label 2 at screen $edgex2,$edgey1 '$kedge2' font '$vfont'
set label 3 at screen $edgex2,$edgey2 '$kedge3' font '$vfont'
set label 4 at screen $edgex1,$edgey2 '$kedge4' font '$vfont'
catend
        fi
        cat >> "$output" <<catend

$cforblabel
plot '<sed "/^#/d" $fsurdat| sort -k4' $psvar palette
catend
      else
        if [ "$lfemptyorbs" -eq 1 ] && [ "$selectorb" -eq 0 ]; then
          echo "set label 2 at screen 0.02,0.02 '{/:Bold *no orbital character above 0.01 for orbitals: $femptyorbs}' font 'times-new-roman, $((opfsize + 4))'" >> "$output"
        fi

        or=0
        while [ "$or" -lt "$fnorbis" ]; do
          if [ "$or" -lt 10 ]; then
            orbitaltitel=$(grep "# $or""ORBITAL=" "$fsurdat" | cut -d'=' -f2)
          else
            orbitaltitel=$(grep "#$or""ORBITAL=" "$fsurdat" | cut -d'=' -f2)
          fi

          #echo "set label 1 at graph 0.45,1.1 '{/:Bold $orbitaltitel}' font '$fotpfont'" >> "$output"
          echo "set label 1 at graph 0.45,1.1 '{/:Bold $orbitaltitel}' font '$pfont'" >> "$output"
          if [ "$florb" = true ]; then
            if sed '/#/d' "$fsurdat" | grep -qE '[0-9]'; then
              echo  "plot '$fsurdat' $forbitvarps1:$((4 + or)) notitle $orbitvarps2 palette" >> "$output"
            fi
          else
            if sed '/#/d' "$fsurdat" | grep -qE '[0-9]'; then
              echo "plot '$fsurdat' $forbitvarps1 notitle $orbitvarps2 lc rgb '$ocolour2'" >> "$output"
            fi
          fi
          echo "" >> "$output"
          or=$((or + 1))
        done
      fi
      echo "unset multiplot" >> "$output"
    fi
  fi
fi

# Actually call gnuplot (redirect output)
gnuplot "$output" > /dev/null 2>&1


#rem=$(sed -n 32p "$tmpdir/$inpar" | grep -iEc '\.?true\.?')
if ! get_inpar_value_by_name 'LEAVEPLOTDATA'; then
  rm -f "$tm1" "$tm2" "$output"
fi

if { [ "$totspin" -eq 2 ] && [ "$spin" = ".spin2" ]; } || [ "$totspin" -eq 1 ]; then
  echo -en "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
  echo "   > Plots created successfully <"
fi

