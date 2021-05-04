#!/bin/bash
# $1=allfilenames
# $2=fstat
# $3=lfermisurface

alfnames="$1"
fstat="$2"
lfermisur="$3"
spin="$4"
if [ $spin -eq 2 ];then
 spn=".spin$5"
else
 spn=''
fi
plotdata=`sed -n 3p "$alfnames"`$spn'.dat'
splotdata=`sed -n 3p "$alfnames"`'.slim'$spn'.dat'
fermifile='../'`sed -n 6p "$alfnames"`$spn
fsurfile=`sed -n 20p "$alfnames"`$spn
fsurspecfile=`sed -n 21p "$alfnames"`$spn

if [ -f $plotdata ];then
  if [ `sed '/#/d' $plotdata|grep -cE '[0-9]'` -gt 0 ];then
    #sort plotdata respect to the bloch character
    sed '/^#/d' $plotdata|awk '{print $3" "$0 }'|sort -n  -k1|awk '{$1="";print $0}' > bloch$plotdata
    orbis=`grep -E "#[ ,1-9][0-9]ORBITAL=*" $plotdata|cut -d'=' -f2`
    norbis=`echo "$orbis"|wc -l`
  fi
fi


if [ -f $splotdata ];then
  if [ `sed '/#/d' $splotdata|grep -cE '[0-9]'` -gt 0 ];then
    sed '/^#/d' $splotdata | sort -n  -k3  > bloch$splotdata

    sed '/^#/d' $splotdata | sort -n  -k4 > orb$splotdata
  fi
fi


fstat=$((2 * $fstat))
if [ -f "$fermifile.temp" ]; then
   grep -E '^#' "$fermifile.temp"  > "$fermifile.dat"
   cat "$fermifile.temp"|sed '/^#/d'| sort -n -k1  >> "$fermifile.dat"
   rm "$fermifile.temp"
else
 fstat=$(($fstat + 1))
fi


#if this is a fermisurface calculation
if $lfermisur;then
if [ -f "$fsurfile.temp" ];then
   grep -E '^#' "$fsurfile.temp"  > "$fsurfile.dat"
   sed '/^#/d' "$fsurfile.temp" | sort -n -k3  >> "$fsurfile.dat"
     rm "$fsurfile.temp"
fi
fi

#check if spectralfunction is activated
lspec=`sed -n 22p "../tempINPAR"|grep -Eic "*true*"` 
if [ $lspec -eq 1 ];then
if [ -f "$fsurspecfile.temp" ];then
   grep -E '^#' "$fsurspecfile.temp"  > "$fsurspecfile.dat"
   sed '/^#/d' "$fsurspecfile.temp"| sort -n -k3  >> "$fsurspecfile.dat"
     rm "$fsurspecfile.temp"
fi
fi

echo $fstat
