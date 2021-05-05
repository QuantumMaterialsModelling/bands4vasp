#!/bin/bash
# $1 = # of inputfiles (pathnumber)
# $2 = directore of default parameter input file

checkplotdefault() {
dpfile="$1"
dpword="$2"

if [ -f "$dpfile" ];then
  lword=`grep -ic "$dpword" $dpfile`
else
  lword=0
fi
if [ $lword -eq 0 ];then
  takefile="$INPAR"
  ldefaultvalue=true
else

 firstdigit=`grep -i "$dpword" "$dpfile" | sed 's/^[ \t]*//' | grep -Eo "^.{1}"`
 if [ "$firstdigit" == '#' ] || [ "$firstdigit" == '!' ];then
  takefile="$INPAR"
  ldefaultvalue=true
 else
  takefile="$dpfile"
  ldefaultvalue=false
 fi
fi

#  defaultvalue=`grep "$dpword" "$dpfile"|cut -d'=' -f2-|sed 's/^[ \t]*//;s/[ \t]*$//'`
#  ldefaultvalue=true
    firstdigit=`grep -i "$dpword" "$takefile" | sed 's/^[ \t]*//' | grep -Eo "^.{1}"`
    if [ "$firstdigit" == '#' ] || [ "$firstdigit" == '!' ];then
      defaultvalue='#'
    else
      defaultvalue=`grep -i "$dpword" "$takefile"|sed -n 1p|cut -d'=' -f2-|sed 's/^[ \t]*//;s/[ \t]*$//'`
    fi
}


checkplotdefaults() {
dpfile="$1"
nextjump=1
for ik in "$@";do
if [ $nextjump -eq 0 ];then
  checkplotdefault "$dpfile" "$ik"
  eval $ik=\$defaultvalue
  eval 'IO'$ik=\$ldefaultvalue
#  echo "$ik = $defaultvalue"
else
  nextjump=0
fi
done
}

INPAR="$1" #directory of default parameter input file
parameter=(EDELTA1 EDELTA2 EDIF BAVERAGE OAVERAGE EGAP BLOCH_THRESHOLD DBLOCH BNDDIFF KAPPANORM GRADIENTD NPOINTS LPOLY REGULAPREC PLOTORB ODISTINCT SYMREC SYMPOINT1 SYMPOINT2 BANDINDEXPLOT SIGMA SPECFUN SPECDELTA SLIMSPEC FGRID SELECTION SKIPKPOINT ROOTSCALC EFERMI MAKEPLOTS LEAVEPLOTDATA PSFAC LFITPOINTS LLINES LROOTS BCOLOURS OCOLOURS BACKCOLOUR FILEFORMAT PLOTSIZE FSURCART PATHPOINTS)
outpar="OUTPAR"
#if [ ! -f "INPAR" ]; then
#cp $INPAR .
#fi
checkplotdefaults "INPAR" "${parameter[@]}"
if [ -f 'tempINPAR' ]; then rm -f 'tempINPAR';fi
echo "$outpar::A list of all used INPAR parameters" > $outpar
echo "--------------------------------------------" >> $outpar

n=0
nall=$((${#parameter[*]} - 1))
#write all in temp$INPAR
while [[ $n -le $nall ]];do
   eval par='$'${parameter[$n]}

   if [ "${parameter[$n]}" == "EDELTA2" ];then
      if $IOEDELTA2&& ! $IOEDELTA1;then  eval par='$'${parameter[$(($n-1))]};fi
   fi
   echo "$par" >> 'tempINPAR'
   echo "${parameter[$n]} = $par" >> $outpar
   if [ "${parameter[$n]}" == "EFERMI" ];then
     if [ $( echo "$par"|grep -icE '^bands$') -eq 1 ];then
       echo 't' >>  'tempINPAR'
     else
       echo 'f' >> 'tempINPAR'
     fi
   fi
   ((n=$n+1))
done
echo "A list of all used parameters was written in $outpar"
