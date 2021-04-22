#!/bin/bash

# $1 orbital name for orbchar plot
# $2 plotdata file
# $3 outgrid file
# $4 name of inpar file
# $5 fermisurface or not
# $6 lfit

# if $lfit -eq 1
# $7 fremi file
# $8 fittpoints
# $9 package directory

lfsurface="$1"
lfit="$2"
packagedir="$3"
allfnames="$4"
created="$5"
totspin="$6"


if [ $totspin -eq 2 ];then
 if [ "$7" -eq 1 ];then
    rm -f $created
    echo -n "            ... creating gnuplot files ..."
 fi
 spin='.spin'"$7"
else
  spin=''
  rm -f $created
  echo -n "            ... creating gnuplot files ..."
fi


inpar=`sed -n 2p "$allfnames"`
plotdata=`sed -n 3p "$allfnames"`
splotdata="$plotdata"'.slim'$spin'.dat'
plotdata="$plotdata$spin"'.dat'
specplotdata=`sed -n 4p "$allfnames"`$spin'.dat'
outgrid=`sed -n 5p "$allfnames"`
fermifile='../'`sed -n 6p "$allfnames"`$spin'.dat'
fittpoints=`sed -n 7p "$allfnames"`$spin
autocolordata=`sed -n 17p "$allfnames"`$spin
fsurdat=`sed -n 20p "$allfnames"`$spin
fsurgrid="$fsurdat.grid.dat"
fsurdat=$fsurdat'.dat'
fsurspecdat=`sed -n 21p "$allfnames"`$spin'.dat'
fsurpm3dspecdat=`sed -n 21p "$allfnames"`$spin'.pm3d.dat'

output=`sed -n 10p "$allfnames"`$spin'.gnu'
tm1=`sed -n 11p "$allfnames"`
tm2=`sed -n 12p "$allfnames"`

imgdir='../'`sed -n 24p "$allfnames"`

bandstructure=$imgdir`sed -n 13p "$allfnames"`$spin
ebsbloch=$imgdir`sed -n 14p "$allfnames"`$spin
ebsorb=$imgdir`sed -n 15p "$allfnames"`
bandindexp=$imgdir`sed -n 16p "$allfnames"`$spin
fsurbloch=$imgdir`sed -n 18p "$allfnames"`$spin
fsurorb=$imgdir`sed -n 19p "$allfnames"`

mkdir -p "$imgdir"

blochpalette="( 0 'white', 0.05 'beige' ,0.25 'greenyellow', 0.4 'web-green', 0.65 'steelblue', 0.9 'midnight-blue')"
orbitpalette="( 0 'gray', 0.1 'beige',0.2 'lemonchiffon', 0.35 'tan1',0.6 'orange-red' ,0.75 'dark-orange' , 0.9 'orangered4')"
ionpalette="( 0 'gray' ,0.09 'pink', 0.35 'dark-pink', 0.7 'dark-violet')"


standartfont='Times-new-roman,14'
titlefont='Times-new-roman,22'
pfont='Times-new-roman,16'
ptemp=`sed -n 1p "../temp$inpar"`
pwind1=$(echo  "$ptemp"|awk '{print $1}')
nbord=$(echo "$ptemp"|grep -Eo '[-]?[0-9]+[\.]?[-]?[0-9]*'|wc -l)
if [ $nbord -gt 1 ];then 
  pwind2=$(echo  "$ptemp"|awk '{print $2}')
else
  pwind2=$pwind1
  pwind1=-$pwind2
fi

ptemp=`sed -n 2p "../temp$inpar"`
fwind1=$(echo  "$ptemp"|awk '{print $1}')
nbord=$(echo "$ptemp"|grep -Eo '[-]?[0-9]+[\.]?[-]?[0-9]*'|wc -l)
if [ $nbord -gt 1 ];then 
  fwind2=$(echo  "$ptemp"|awk '{print $2}')
else
  fwind2=$fwind1
  fwind1=-$fwind2
fi


psfac=`sed -n 33p "../temp$inpar"`

userbpalette=false
bcolour1=`sed -n 37p  "../temp$inpar"|awk '{print $1}'`
if [ "$bcolour1" != '#' ];then
bcolour2=`sed -n 37p  "../temp$inpar"|awk '{print $2}'`
userbpalette=true
else
bcolour2='blue'
fi



useropalette=false
ocolour1=`sed -n 38p "../temp$inpar"|awk '{print $1}'`
if [ "$ocolour1" != '#' ];then
  ocolour2=`sed -n 38p "../temp$inpar"|awk '{print $2}'`
  useropalette=true
else
  ocolour2='red'
fi


background=`sed -n 39p "../temp$inpar"|awk '{print $1}'`
bandplot=`sed -n 20p "../temp$inpar"|grep -Eic "*true*"`
iend=`grep '#KPOINTgrid' "$outgrid"|awk '{print $2}'`
#Files initializations
llines=`sed -n 35p "../temp$inpar"|grep -Eic "*default*"`

if [ $llines -eq 1 ];then
  llinesraw=0
  llinesmani=1
else
  llines=`sed -n 35p "../temp$inpar"|grep -Eic "*true*"`
  llinesraw=$llines
  llinesmani=$llines
fi

lroots=`sed -n 36p "../temp$inpar"|grep -Eic "*default*"`

if [ $lroots -eq 1 ];then
  lrootsraw=0
  lrootsmani=1
else
  lroots=`sed -n 35p "../temp$inpar"|grep -Eic "*true*"`
  lrootsraw=$lroots
  lrootsmani=$lroots
fi

lpoints=`sed -n 34p "../temp$inpar"|grep -Eic "*default*"`

if [ $lpoints -eq 1 ];then
  lpointsraw=0
  lpointsmani=1
else
  lpoints=`sed -n 35p "../temp$inpar"|grep -Eic "*true*"`
  lpointsraw=$lpoints
  lpointsmani=$lpoints
fi



lspec=`sed -n 22p "../temp$inpar"|grep -Eic "*true*"`
lspecpm3d=`sed -n 24p "../temp$inpar"|grep -Eic "*true*"`
fgrid=`sed -n 25p "../temp$inpar"|awk '{print $1}'`
rocalc=`sed -n 28p "../temp$inpar"|grep -Eic "*true*"`
x0=`grep "#1 " "$outgrid"|awk '{print $2}'`
x1=`grep "#$iend " "$outgrid"|awk '{print $2}'`

cros0=`tail -n1 "../temp$inpar"`


lxrange=true
if [ "$x0" == "$x1" ];then lxrange=false;fi
rm -f $output










##############################################################
################# figure out fileformat ######################
##############################################################

#inparplotsize=`grep -i 'PLOTSIZE' "$inpar"|sed 's/^[ \t]*//'`
inparplotsize=`sed -n 41p "../temp$inpar"|sed 's/^[ \t]*//'`

if [ -z "$inparplotsize" ]||[ `echo "$inparplotsize"|grep -Ec '^#'` -eq 1 ];then
  plotsize=''
else
#  plotsize="size `echo "$inparplotsize"|cut -d'=' -f2-`"
  plotsize="size $inparplotsize"
fi

fileformat=`sed -n 40p "../temp$inpar"|awk '{print $1}'`


#if [ `echo "$fileformat"|grep -Eio 'pdf'|wc -l` -eq 1 ];then
#  if [ -z "$plotsize" ];then
#     terminal="set terminal pdf size 900,700 font '$standartfont'"
#  else
#    terminal="set terminal pdf $plotsize font '$standartfont'"
#  fi
#  prefix='pdf'
#==================== png ========================
if [ `echo $fileformat|grep -Eio '^png$'|wc -l` -eq 1 ];then

  titlefont='Times-new-roman,34'
  standartfont='Times-new-roman,22'
  pfont='Times-new-roman,28'


  if [ -z "$plotsize" ];then
    terminal="set terminal png size 1500,1125 enhanced font '$standartfont'"
  else
    terminal="set terminal png $plotsize enhanced font '$standartfont'"
  fi
  prefix='png'
  ylabel="set ylabel 'E - E_F (eV)' font '$titlefont'" 
 
  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ];then cbblochoffset="$cbblochoffset offset graph -0.095,graph -0.45";fi
  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})' font '$titlefont'"
  if [ -z "$plotsize" ];then cbspeclabel="$cbspeclabel"' offset graph -0.093,graph -0.04';fi
  cborblabel="font '$titlefont'"
  if [ -z "$plotsize" ];then cborblabel="$cborblabel"' offset graph -0.094,graph -0.25';fi


  cforblabel="font '$titlefont' offset graph -0.1,graph -0.17 rotate by 90"
  cforbspeclabel="set cblabel 'A({/:Bold k},E_{F})' font '$titlefont' offset graph -0.08, graph 0.5 rotate by 0"

  pstemp=$(echo "scale=3;0.9*$psfac"|bc)
  statfp=`echo "scale=2; 2.2 * $psfac"|bc`
  fstatfp=`echo "scale=2; 2.0 * $psfac"|bc`
  bstatfp=`echo "scale=2; 3.4 * $psfac"|bc`



#============ png cairo ==================
elif [ `echo $fileformat|grep -Eio '^pngcairo$'|wc -l` -eq 1 ];then
  titlefont='Times-new-roman,34'
  standartfont='Times-new-roman,22'
  pfont='Times-new-roman,28'
  if [ -z "$plotsize" ];then
    terminal="set terminal pngcairo size 1500,1125 enhanced font '$standartfont'"
  else
    terminal="set terminal pngcairo $plotsize enhanced font '$standartfont'"
  fi
  prefix='png'

  ylabel="set ylabel 'E - E_F (eV)' font '$titlefont'" 
 
  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ];then cbblochoffset="$cbblochoffset offset graph -0.095,graph -0.45";fi
  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})' font '$titlefont'"
  if [ -z "$plotsize" ];then cbspeclabel="$cbspeclabel"' offset graph -0.093,graph -0.04';fi
  cborblabel="font '$titlefont'"
  if [ -z "$plotsize" ];then cborblabel="$cborblabel"' offset graph -0.094,graph -0.25';fi

  cforblabel="font '$titlefont' offset graph -0.1,graph -0.17 rotate by 90"
  cforbspeclabel="set cblabel 'A({/:Bold k},E_{F})' font '$titlefont' offset graph -0.08, graph 0.5 rotate by 0"

  pstemp=$(echo "scale=3;0.9*$psfac"|bc)
  statfp=`echo "scale=2; 2.2 * $psfac"|bc`
  fstatfp=`echo "scale=2; 2.0 * $psfac"|bc`
  bstatfp=`echo "scale=2; 3.4 * $psfac"|bc`

#elif [ `echo "$fileformat"|grep -Eio 'epslatex'|wc -l` -eq 1 ];then
#  terminal="set terminal epslatex $plotsize font '$standartfont'"
#  prefix='tex'
#elif [ `echo "$fileformat"|grep -Eio 'cairolatex'|wc -l` -eq 1 ];then
#  if [ -z "$plotsize" ];then
#    terminal="set terminal cairolatex pdf size 900,700 font '$standartfont'"
#  else
#     terminal="set terminal cairolatex pdf $plotsize font '$standartfont'"
#  fi
#  prefix='pdf'
else
  titlefont='Times-new-roman,22'
  standartfont='Times-new-roman,14'
  pfont='Times-new-roman,16'
  terminal="set terminal postscript eps $plotsize enhanced color font '$standartfont'"
 
  ylabel="set ylabel 'E - E_F (eV)' font '$titlefont'" 
  prefix='eps'
  cbblochoffset="set cblabel '{/:Bold P_{Km}}' font '$titlefont'"
  if [ -z "$plotsize" ];then cbblochoffset="$cbblochoffset offset graph -0.114,graph -0.45";fi
  cbspeclabel="set cblabel '{/:Bold A}({/:Bold k},E_{F})' font '$titlefont'"
  if [ -z "$plotsize" ];then cbspeclabel="$cbspeclabel"' offset graph -0.11,graph -0.04';fi
  cborblabel="font '$titlefont'"
  if [ -z "$plotsize" ];then cborblabel="$cborblabel"' offset graph -0.113,graph -0.25';fi

  cforblabel="font '$titlefont' offset graph -0.125,graph -0.145 rotate by 90"
  cforbspeclabel="set cblabel 'A({/:Bold k},E_{F})' font '$titlefont' offset graph -0.125, graph 0.55 rotate by 0"


  pstemp=$(echo "scale=3;0.7*$psfac"|bc)
  statfp=`echo "scale=2; 0.9 * $psfac"|bc`
  fstatfp=`echo "scale=2; 0.8 * $psfac"|bc`
  bstatfp=`echo "scale=2; 1.6 * $psfac"|bc`
fi












#########################################
#######  Make Grid and Xtics  ###########
#########################################

#Take pathnames from INPAR
temppath=`sed -n 42p "../temp$inpar"`


#if pathnames are commented show x values
if [ "$temppath" == '#' ];then
  temppath=""
  xtics=""
  xlabel="set xlabel 'k-points distance' font '$titlefont'"
else
  i=1
  while [[ $i -le $iend ]];do
    pntemp=`echo "$temppath" | awk '{print $1}'`
    if [ `echo "$pntemp"|grep -ic '^/'` -eq 1 ];then
      pntemp=`echo "$pntemp"|sed 's/\///'`
      pathname[$i]="{/Symbol $pntemp}"
    else
      pathname[$i]="$pntemp"
    fi
    temppath=`echo "$temppath" | awk '{$1="";print $0}'`
    ((i=$i+1))
  done

  #Setting distances and printing KPOINTS grid file
  istart=1
  for i in $(seq $istart $iend);do
    gridklength=`grep "#$i " $outgrid | awk '{print $2 }'`
    if [ $i -eq $iend ];then
      xtics=`echo  "$xtics, "'"'"${pathname[$i]}"'"'" $gridklength)"`
    elif [ $i -eq $istart ];then
      xtics=`echo -n "("'"'"${pathname[$i]}"'"'" $gridklength"`
    else
      xtics=`echo -n "$xtics, "'"'"${pathname[$i]}"'"'" $gridklength"`
    fi
  done
  xlabel=""
fi









################# preperation ###################

################ for orbital plots ##############
#get orbital accuracy => true or false
if [ $lfit -lt 6 ];then
  #grep the orbital name for the orbchar plot
  orbname=`grep -E "#[ ,1-9][0-9]ORBITAL=*" $plotdata|cut -d'=' -f2`
  maxorb=`grep '#maxorb ' $plotdata|awk '{print $2}'`
  norbis=`echo "$orbname"|wc -l`
  if [ $norbis -gt 1 -o $norbis -lt 1 ]||[ -z `echo $orbname` ];then orbname='ALL';fi
  lemptyorbs=0
  if [ $norbis -gt 1 ];then
     lemptyorbs=`grep -c "#emptyorbs " $plotdata`
     if [ $lemptyorbs -eq 1 ];then emptyorbs=`grep "#emptyorbs " $plotdata|cut -d' ' -f2-`;fi
  fi
fi

orbis=`grep -E "#[ ,1-9][0-9]ORBITAL=*" $plotdata|cut -d'=' -f2`
norbis=`echo "$orbis"|wc -l`
fnorbis=`echo "$forbis"|wc -l`
topfsize=`echo "$pfont"|cut -d',' -f2|awk '{print $0}'`
if [ $norbis -gt 1 ];then
 orblayout="layout `grep '#LAYOUT=' $plotdata|cut -d'=' -f2` margins 0.05,0.92,0.09,0.95 spacing 0.11,0.08"
  ops=`echo "scale=3; 0.7-(1.0/$norbis.0)"|bc`
  opfsize=`echo "scale=1; $topfsize-(14*$ops)/1"|bc`
  opfont=`echo "$pfont"|cut -d',' -f1|awk '{print $0}'`",$opfsize"
  if [ -n "$xlabel" ];then xlabel="";fi
  ebsorbit="$ebsorb"'_ALL'$spin
  orbname=`echo "$orbis"|sed -n 1p`
  fsurorbit="$fsurorb"'_'"$orbname"$spin
else
  ebsorbit="$ebsorb"'_'"$orbname"$spin
  orblayout=''
  ops=0.0
  opfont="$pfont"
  opfsize=$topfsize
fi



cborblabel="set cblabel '{/:Bold Orbitalcharacter $orbname}' $cborblabel"

cforblabel="set cblabel '{/:Bold Interpolated orbitalcharacter $orbname}' $cforblabel"

if [ -f $fsurdat ];then
forbis=`grep -E "#[ ,1-9][0-9]ORBITAL=*" $fsurdat|cut -d'=' -f2`
if [ $fnorbis -gt 1 ];then
  forblayout="layout `grep '#LAYOUT=' $fsurdat|cut -d'=' -f2`"
  fops=`echo "scale=3; 0.7-(1.0/$fnorbis.0)"|bc`
  fopfsize=`echo "scale=1; $topfsize-(9*$fops)/1"|bc`
  fopfont=`echo "$pfont"|cut -d',' -f1|awk '{print $0}'`",$fopfsize"
  fsurorbit="$fsurorb"'_ALL'$spin
else
  fsurorbit="$fsurorb"'_'"$orbname"$spin
  forblayout=''
  fops=0.0
  fopfont="$pfont"
  fopfsize=$topfsize
fi

if [ `grep -c '#noorbital' $fsurdat` -eq 0 ];then
	florb=true
	fno=''
else
	florb=false
	fno='No'
	forbname=""
fi
fi


if [ `cat $splotdata|grep -c '#noorbital'` -eq 0 ];then
	slorb=true
	sno=''
	sorbname="$orbname"
else
	slorb=false
	sno='No'
	sorbname=""
fi

if [ `grep -c '#noorbital' $plotdata` -eq 0 ];then
	lorb=true
	no=''
else
	lorb=false
	no='No'
	orbname=""
fi





#PROCAR file
if [ $lfit -eq 4 -o $lfit -eq 5 ]; then
  orbitvarps1='using 1:2'
  orbitvarps1s='using 1:2'
  forbitvarps1s='using 4:(0)'
  fovarps1='using 4:(0)'
  orbitvarps2='with points pt 7 ps '`echo "scale=3; $psfac * ($statfp-$ops)"|bc`
  fovarps2='with points pt 7 ps '`echo "scale=3; $psfac * (0.2+$statfp-$ops)"|bc`


#PROCAR.prim or PRJCAR file
else
  orbitvarps1='using 1:2:($3+'`echo "scale=3;$psfac * ($statfp-0.5-$ops)"|bc`')'
  orbitvarps1s='using 1:2:($3+'`echo "scale=3;$psfac * ($statfp-0.2)"|bc`')'
  if $slorb;then
forbitvarps1s="using 4:(0):(\$6+0.5+($psfac *$statfp))"
  else
	  forbitvarps1s="using 4:(0):(\$6+($psfac * ($statfp - 0.2)))"
  fi
  fovarps1='using 4:(0):($6+'`echo "scale=2;$psfac * ($statfp-0.4-$ops)"|bc`')'
  orbitvarps2='with points pt 7 ps var'
  fovarps2='with points pt 7 ps var'
fi

if [ $lroots -eq 1 -a $(($lfit%2)) -ne 0 -a $rocalc -eq 1 -a $cros0 -eq 1 ];then
   kdist=`sed -n 1p "$fittpoints.dat"`
   kmid=`sed -n 2p "$fittpoints.dat"`
   egap1=`sed -n 3p "$fittpoints.dat"`
   egap2=`sed -n 4p "$fittpoints.dat"`
   egap=`sed -n 5p "$fittpoints.dat"`
   emid=`sed -n 6p "$fittpoints.dat"`
   segap1=`sed -n 7p "$fittpoints.dat"`
   segap2=`sed -n 8p "$fittpoints.dat"`
   segap=`sed -n 9p "$fittpoints.dat"`
   semid=`sed -n 10p "$fittpoints.dat"`
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
gnuplot -e 'show palette colornames' > color_table.temp 2>&1
colnums=`grep -E 'There are [0-9]+ predefined color names:' color_table.temp|grep -oE '[0-9]+'`
if [ -n "$colnums" ];then
  n=1
  nshift=`grep -nE 'There are [0-9]+ predefined color names:' color_table.temp|cut -d':' -f1`
  while [ $n -le $colnums ];do
    let line=n+nshift
    color_table[$n]="$(sed -n $line'p' color_table.temp|awk '{print $1}')"
    let n++
  done
else
  n=1
  colnums=`cat "$packagedir/gnuplot_color_table"|wc -l`
  while [ $n -le $colnums ];do
    color_table[$n]="$(sed -n $n'p' "$packagedir/gnuplot_color_table")"
    let n++
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

echo "$terminal" > $output
if [ $lfit -eq 4 -o $lfit -eq 5 ]; then
  echo "set out '$bandstructure.$prefix'" >> $output
  echo "$bandstructure.$prefix" >> $created
else
  echo "set out '$ebsbloch.$prefix'" >> $output
  echo "$ebsbloch.$prefix" >> $created
  if $userbpalette;then
    echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> $output
  else
    echo "set palette defined $blochpalette" >> $output
  fi
fi

echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> $output
echo "set object 1 fc rgb '$background' fs solid 1.0 behind" >> $output
if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi
cat >> $output <<catOUT
set yrange [$pwind1:$pwind2]
unset key
$xlabel
set ylabel "E - E_F (eV)" font "$titlefont" offset graph 0.01
set format "%4.2g"
set cbrange [0.0:1.0]
set cbtics font "$pfont"
$cbblochoffset
set multiplot
set xtics $xtics font "$pfont"
set ytics font "$pfont"
$fgap1
$fgap2
$fgap3
p [][]\\
catOUT
if [ $lfit -eq 4 -o $lfit -eq 5 ];then
  if [ `sed '/#/d' $plotdata|grep -cE '[0-9]'` -gt 0 ];then
    echo " '$plotdata' u 1:2 w p pt 7 ps $statfp lc rgb '$bcolour2',"'\' >> $output
  fi
else
  if [ `sed '/#/d' $plotdata|grep -cE '[0-9]'` -gt 0 ];then
    echo " 'bloch$plotdata' u 1:2:3 w p pt 7 ps $statfp palette,"'\' >> $output
  fi
fi
echo -n "'$outgrid' w l ls 0, 0 ls 0" >> $output
if [ $(($lfit%2)) -eq 0 ]; then
  if [ $llinesraw -eq 1 ];then
   echo ',\' >> $output
   echo -n "'$fittpoints.lines.dat' u 1:2  w l lt 2 lw $bstatfp lc rgb 'orange'" >> $output
  fi
  if [ $lpointsraw -eq 1 ];then
   echo ',\' >> $output
   echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $fstatfp lc rgb 'orange'" >> $output
  fi

  if [ $lrootsraw -eq 1 ];then
   echo ',\' >> $output
   echo -n "'$fermifile' u 4:(0) w p pt 7 ps $statfp lc rgb 'dark-orange'" >> $output
  fi
fi
echo "" >> $output


if ! $lfsurface&&[ $lspec -eq 1 ]&&[ `sed '/#/d' $specplotdata |grep -cE '[0-9]'` -gt 0 ];then
######################################################################
################### Bloch spectral function ##########################
######################################################################


cbmax=$(sort -k4 $specplotdata|tail -n1|awk '{print $4}')

cat >> $output <<catOUT
unset multiplot
reset
unset key





$terminal
catOUT


if [ $lfit -eq 4 -o $lfit -eq 5 ]; then
  echo "set out '$bandstructure.spec.$prefix'" >> $output
  echo "$bandstructure.spec.$prefix" >> $created
else
  echo "set out '$ebsbloch.spec.$prefix'" >> $output
  echo "$ebsbloch.spec.$prefix" >> $created
fi


echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> $output
echo "set object 1 fc rgb 'black' fs solid 1.0 behind" >> $output
if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi
cat >> $output <<catOUT
set yrange [$pwind1:$pwind2]
set multiplot
$xlabel
set ylabel "E - E_F (eV)" font "$titlefont"
set format "%4.2g"
set cbtics font "$pfont"
$cbspeclabel
set cbrange [0.0:$cbmax]
set xtics $xtics font "$pfont"
set ytics font "$pfont"
p [][]\\
 '<sort -k4 $specplotdata' u 2:3:4 w p pt 7 ps $pstemp palette, \\
 '$outgrid' w l ls 0 lc rgb 'white', 0 ls 0 lc rgb 'white'

catOUT
fi


#################################################################
#----------------------------------------------------------------
#============== EBSbloch.fine / Bandstructure.fine ==============
#----------------------------------------------------------------

cat >> $output <<catOUT
unset multiplot
reset
unset key




$terminal
catOUT

if [ $lfit -eq 4 -o $lfit -eq 5 ]; then
  echo "set out '$bandstructure.manipulated.$prefix'" >> $output
  echo "$bandstructure.manipulated.$prefix" >> $created
else
  echo "set out '$ebsbloch.manipulated.$prefix'" >> $output
  echo "$ebsbloch.manipulated.$prefix" >> $created
  if $userbpalette;then
    echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> $output
  else
    echo "set palette defined $blochpalette" >> $output
  fi
fi

echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> $output
echo "set object 1 fc rgb '$background' fs solid 1.0 behind" >> $output
if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi
cat >> $output <<catOUT
set yrange [$fwind1:$fwind2]
$xlabel
#set ylabel "E - E_F (eV)" font "$titlefont" offset graph -0.01
$ylabel
set format "%4.2g"
set cbrange [0.0:1.0]
set cbtics font "$pfont"
$cbblochoffset
set multiplot
set xtics $xtics font "$pfont"
set ytics font "$pfont"
$sfgap1
$sfgap2
$sfgap3
p [][]\\
catOUT
if [ $lfit -eq 4 -o $lfit -eq 5 ];then
  if [ `sed '/#/d' $splotdata|grep -cE '[0-9]'` -gt 0 ];then
    echo " '$splotdata' u 1:2 w p pt 7 ps $statfp lc rgb '$bcolour2',"'\' >> $output
  fi
else
  if [ `sed '/#/d' bloch$splotdata|grep -cE '[0-9]'` -gt 0 ];then
    echo " 'bloch$splotdata' u 1:2:3 w p pt 7 ps $statfp palette,"'\' >> $output
  fi
fi
echo -n "'$outgrid' w l ls 0, 0 ls 0" >> $output
if [ $(($lfit%2)) -eq 0 ];then
  if [ $llinesmani -eq 1 ];then
    echo ',\' >> $output
    echo -n "'$fittpoints.lines.dat' u 1:2  w l lt 2 lw $bstatfp lc rgb 'orange'" >> $output
  fi
  if [ $lpointsmani -eq 1 ];then
   echo ',\' >> $output
   echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $bstatfp lc rgb 'orange'" >> $output
   if [ $lfit -lt 4 -o $lfit -gt 5 ];then
      echo ',\' >> $output
      echo -n "'$fittpoints.dat' u 1:2:3 w p pt 7 ps $statfp palette" >> $output
   fi
  fi

  if [ $lrootsmani -eq 1 ];then
    echo ',\' >> $output
    echo -n "'$fermifile' u 4:(0) w p pt 7 ps $bstatfp lc rgb 'dark-orange'" >> $output
    if [ $lfit -lt 4 -o $lfit -gt 5 ];then
      echo ',\' >> $output
      echo -n "'$fermifile' u 4:(0):6 w p pt 7 ps $statfp palette" >> $output
     fi
   fi
fi
echo "" >> $output

if [ $lfit -lt 6 ];then
#################################################################
#----------------------------------------------------------------
#============================= EBSorbit =========================
#----------------------------------------------------------------
echo "$ebsorbit.$prefix" >> $created
cat >> $output <<catOUT
unset multiplot
reset





$terminal
set out '$ebsorbit.$prefix'

unset key


set multiplot $orblayout

set xtics $xtics font "$opfont"
set ytics font "$opfont"
set format "%4.2g"
set cbtics font "$opfont"
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
set cbrange [0:$maxorb]
set yrange [$pwind1:$pwind2]
catOUT
if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi

if $useropalette;then
 echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> $output
else
 echo "set palette defined $orbitpalette" >> $output
fi

if $lorb;then

if [ $norbis -eq 1 ];then
cat >> $output <<catOUT
$xlabel
set ylabel "E - E_F (eV)" font "$titlefont"
$cborblabel
$fgap1
$fgap2
$fgap3
catOUT
fi
fi
or=0


if [ $norbis -gt 1 -a $lemptyorbs -eq 1 ];then
echo "set label 2 at screen 0.02,0.02 '{/:Bold *no orbital character above 0.01 for orbitals: $emptyorbs}' font 'times-new-roman, `echo "$opfsize + 4"|bc`'" >> $output
fi

while [ $or -lt $norbis ];do

orbitaltitel=`grep $or"ORBITAL=" $plotdata|cut -d'=' -f2`
echo "set label 1 at graph 0.5,1.06 '{/:Bold $orbitaltitel}' font 'times-new-roman, `echo "$opfsize + 3"|bc`'" >> $output
echo 'p [][]\' >> $output
#if $lorb;then
#  if [ `sed '/#/d' orb$or$plotdata|grep -cE '[0-9]'` -gt 0 ];then
#    echo "'orb$or$plotdata' $orbitvarps1:$((4 + $or)) $orbitvarps2 palette,"'\' >> $output
#  fi
#else
#  if [ `sed '/#/d' orb$or$plotdata|grep -cE '[0-9]'` -gt 0 ];then
#    echo "'orb$or$plotdata' $orbitvarps1 $orbitvarps2 lc rgb '$ocolour2',"'\' >> $output
#  fi
#fi

#echo "set label 1 at graph 0.5,1.06 '{/:Bold $orbitaltitel}' font 'times-new-roman, `echo "$opfsize + 3"|bc`'" >> $output
if $lorb;then
  if [ `sed '/#/d' $plotdata|grep -cE '[0-9]'` -gt 0 ];then
echo "'<sed "/^#/d" $plotdata|sort -n -k$((4 + $or))' $orbitvarps1:$((4 + $or)) $orbitvarps2 palette,"'\' >> $output
  fi
else
  if [ `sed '/#/d' $plotdata|grep -cE '[0-9]'` -gt 0 ];then
    echo "'<sed "/^#/d" $plotdata|sort -n -k$((4 + $or))' $orbitvarps1 $orbitvarps2 lc rgb '$ocolour2',"'\' >> $output
  fi
fi



echo -n "'$outgrid' w l ls 0, 0 ls 0" >> $output
if [ $(($lfit%2)) -eq 0 -a $norbis -eq 1 ];then
  if [ $llinesraw -eq 1 ];then
    echo ',\' >> $output
    echo -n "'$fittpoints.lines.dat' u 1:2 w l lt 2 lw $bstatfp lc rgb 'green'" >> $output
  fi
  if [ $lpointsraw -eq 1 ];then
   echo ',\' >> $output
   echo -n "'$fittpoints.dat' $orbitvarps1 $orbitvarps2 lc rgb 'green'" >> $output
  fi
  if [ $lrootsraw -eq 1 ];then
    echo ',\' >> $output
    echo -n "'$fermifile' u 4:(0) w p pt 7 ps 1.0 lc rgb 'blue'" >> $output
  fi
fi
echo "" >> $output
let or=$or+1
done

#################################################################
#----------------------------------------------------------------
#======================== EBSorbit.fine =========================
#----------------------------------------------------------------

if [ $norbis -eq 1 ];then
echo "$ebsorbit.manipulated.$prefix" >> $created
cat >> $output <<catOUT
unset multiplot
reset





$terminal
set out '$ebsorbit.manipulated.$prefix'
unset key
$xlabel
set ylabel "E - E_F (eV)" font "$titlefont" offset graph 0.01
set multiplot
set ytics font "$pfont"
set xtics $xtics font "$pfont"
set yrange [$fwind1:$fwind2]
catOUT
if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi

echo "set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back" >> $output
echo "set object 1 fc rgb '$background' fs solid 0.5 behind" >> $output

if $slorb;then

#cblabel="set cblabel '{/:Bold Orbitalcharacter $orbname}' font '$titlefont'"
#if [ -z "$plotsize" ];then cblabel="$cblabel"' offset graph -0.113,graph -0.27';fi
if $useropalette;then
  echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> $output
else
  echo "set palette defined $orbitpalette" >> $output
fi

cat >> $output <<catOUT
set format "%4.2g"
set cbtics font "$pfont"
$cborblabel
$sfgap1
$sfgap2
$sfgap3
catOUT
fi
echo "plot [][]"'\' >> $output
if $slorb;then

  if [ `sed '/#/d' orb$splotdata|grep -cE '[0-9]'` -gt 0 ];then
  echo "'orb$splotdata' $orbitvarps1s:4 $orbitvarps2 palette," '\' >> $output
  fi
else
  if [ `sed '/#/d' orb$splotdata|grep -cE '[0-9]'` -gt 0 ];then
  echo "'orb$splotdata' $orbitvarps1s $orbitvarps2 lc rgb '$ocolour2'," '\' >> $output
  fi
fi
echo -n "'$outgrid' w l ls 0, 0 ls 0" >> $output
if [ $(($lfit%2)) -eq 0 ];then
  if [ $llinesmani -eq 1 ];then
    echo ',\' >> $output
    echo -n "'$fittpoints.lines.dat' u 1:2 w l lt 2 lw $bstatfp lc rgb 'green'" >> $output
  fi
  if [ $lpointsmani -eq 1 ];then
   if [ $lfit -lt 4 -o $lfit -gt 5 ];then
     echo ',\' >> $output
     echo -n "'$fittpoints.dat' u 1:2:(\$3+0.2+$psfac*$statfp) w p pt 7 ps var lc rgb 'green'" >> $output
     if $slorb;then
       echo ',\' >> $output
       echo -n "'$fittpoints.dat' u 1:2:(\$3+$psfac*($statfp-0.4)):4 w p pt 7 ps var palette" >> $output
     fi
   else
     echo ',\' >> $output
     echo -n "'$fittpoints.dat' u 1:2 w p pt 7 ps $statfp lc rgb 'green'" >> $output
     if $slorb;then
       echo ',\' >> $output
       echo -n "'$fittpoints.dat' u 1:2:4 w p pt 7 ps $fstatfp palette" >> $output
     fi
   fi
  fi
  if [ $lrootsmani -eq 1 ];then
   if [ `sed '/#/d' $fermifile|grep -cE '[0-9]'` -gt 0 ];then
    echo ',\' >> $output
echo -n "'$fermifile' $forbitvarps1s $fovarps2 lc rgb 'blue'" >> $output
    if $slorb;then
      echo ',\' >> $output
      echo -n "'$fermifile' $fovarps1:7 $orbitvarps2 palette" >> $output
    fi
   fi
  fi
fi

echo "" >> $output
fi
fi #norbis -eq 1


#########################################################################
#=======================================================================#
#============================ Bandindexplot ============================#
#=======================================================================#
#########################################################################
if [ $bandplot -eq 1 ];then
echo "$bandindexp.$prefix" >> $created
cat >> $output <<catOUT
unset multiplot
reset





$terminal
set out '$bandindexp.$prefix'
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
catOUT
  if $lxrange;then echo "set xrange [$x0:$x1]" >> $output;fi
cat >> $output <<catOUT
set yrange [$pwind1:$pwind2]
unset key
$xlabel
set ylabel "E - E_F (eV)" font "$titlefont"
set format "%4.2g"
set multiplot
set xtics $xtics font "$pfont"
set ytics font "$pfont"
set key title 'Bandindex'
set key outside right top box 3
$fgap1
$fgap2
$fgap3
p [][]\\
catOUT
cn=1
for bpf in `ls bandindexplot/ |grep -Eo "$autocolordata.[0-9]{1,4}.dat"`;do

if [ $cn -ge $nctable ];then cn=1;fi
if [ `echo "$background"|grep -Eic "^${color_table[$cn]}$"` -eq 1 ];then
	cn=$(($cn + 1))
fi
if [ $cn -ge $nctable ];then cn=1;fi
  if [ `sed '/#/d' './bandindexplot/'$bpf|grep -cE '[0-9]'` -gt 0 ];then
bpftitle=`echo "$bpf"|grep -Eo '[0-9]{1,4}.dat$'|grep -Eo '[0-9]{1,4}'`

 if [ $lfit -eq 4 -o $lfit -eq 5 ];then
   echo " './bandindexplot/$bpf' u 1:2 title '$bpftitle' w lines lw 2 lc rgb '${color_table[$cn]}',"'\' >> $output
 else
   echo " './bandindexplot/$bpf' u 1:2:(\$3+$statfp-0.5) title '$bpftitle' w p pt 7 ps var lc rgb '${color_table[$cn]}',"'\' >> $output
 fi
  fi
cn=$(($cn + 1))
done
echo "'$outgrid' notitle w l ls 0, 0 notitle ls 0" >> $output
echo "" >> $output

fi



#################################################################
#----------------------------------------------------------------
#========================= Fermisurface =========================
#----------------------------------------------------------------

#if $lfsurface&&[ $(( $lfit % 2 )) -eq 0 ]&&[ -f $fsurdat ];then
if $lfsurface;then

  psvar="1:2:4 w p pt 7 ps 0.7*$psfac"

cat >> $output <<catend
reset
unset key
unset multiplot
catend

   if [ $lfit -lt 4 -o $lfit -gt 5 ]; then

if [ -f $fsurdat -a $rocalc -eq 1 ]&&[ $(( $lfit % 2 )) -eq 0 ];then
  if [ `sed '/#/d' $fsurdat|grep -cE '[0-9]'` -gt 0 ];then

   fxb1=`grep '#x ' "$fsurdat" |awk '{print $2}'`
  fxb2=`grep '#x ' "$fsurdat" |awk '{print $3}'`
  fyb1=`grep '#y ' "$fsurdat" |awk '{print $2}'`
  fyb2=`grep '#y ' "$fsurdat" |awk '{print $3}'`     

	  echo "$fsurbloch.$prefix" >> $created
    #PROCAR.pirm or PRJCAR file (Blochcharacter)
         psvar="1:2:($psfac*$statfp*\$3+0.5):4 w p pt 7 ps var"
cat >> $output <<catend




$terminal
set out '$fsurbloch.$prefix'
set size ratio 1
#set palette rgbformulae 34,35,36
set cblabel "~{/:Bold P}^{0.8/:Bold \\\\~}_~{/:Bold Km}" font '$titlefont' offset graph -0.125, graph 0.54 rotate by 0
set cbrange [0.0:1.0]
set xrange [$fxb1:$fxb2]
set yrange [$fyb1:$fyb2]
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb '$background' fillstyle solid 1.0 behind
set tmargin 3
catend

  if $userbpalette;then
    echo "set palette defined (0 '$bcolour1', 1 '$bcolour2')" >> $output
  else
    echo "set palette defined $blochpalette" >> $output
  fi

if [ $fgrid -gt 0 ];then
cat >> $output <<catend
plot '$fsurdat' u 1:2:3 w p pt 7 ps 0.7*$psfac palette,\\
   '$fsurgrid' notitle w l ls 0 lc rgb 'white'
catend
else
  echo "plot '$fsurdat' u 1:2:3 w p pt 7 ps $statfp*$psfac palette" >> $output
fi


cat >> $output <<catend

unset multiplot
catend

fi
fi

#=============================================================================
#========================== Spectral Fermisurface ============================
#=============================================================================
if [ $lspec -eq 1 ];then
  if [ -f $fsurspecdat -a $lspecpm3d -eq 1 ]||[ $lspecpm3d -eq 0 -a -f $fsurpm3dspecdat ];then

if [ `sed '/#/d' $fsurspecdat|grep -cE '[0-9]'` -gt 0 ];then

echo "$fsurbloch.spec.$prefix" >> $created

cat >> $output <<catend



$terminal
set out '$fsurbloch.spec.$prefix'
set size ratio 1
set palette
set cbrange [0.0:1.0]
set cblabel "A({/:Bold k},E_{F})" font '$titlefont' offset graph -0.1, graph 0.54 rotate by 0
set xrange [$fxb1:$fxb2]
set yrange [$fyb1:$fyb2]
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb "black" fillstyle solid 1.0
unset multiplot
catend

if [ $lspecpm3d -eq 0 ];then

cat >> $output <<catend
set pm3d map
set pm3d interpolate 0,0
#set samples 10
catend

echo -n "splot '$fsurpm3dspecdat' u 1:2:3 w pm3d" >> $output

else
echo -n "plot '$fsurspecdat' u 1:2:3 w p pt 7 ps ($statfp-0.4)*$psfac palette" >> $output

fi


if [ $fgrid -gt 0 ];then
echo ',\' >> $output

echo  "'$fsurgrid' notitle w l ls 0 lc rgb 'white'" >> $output
else
	echo " " >> $output
fi


fi
fi
fi
fi

#======================================================================================
#=============================== Orbital Fermisurface =================================
#======================================================================================
#PROCAR[.prim] file (orbitalcharacter)
if [ $lfit -lt 6 ]; then

if [ -f $fsurdat -a $rocalc -eq 1 ]&&[ $(( $lfit % 2 )) -eq 0 ];then
if [ `sed '/#/d' $fsurdat|grep -cE '[0-9]'` -gt 0 ];then
echo "$fsurorbit.$prefix" >> $created
cat >> $output <<catend
reset
unset key



set out '$fsurorbit.$prefix'
set size ratio 1
set multiplot $forblayout
set xrange [$fxb1:$fxb2]
set yrange [$fyb1:$fyb2]
set format "%4.2g"
set cbrange [0.0:1.0]
set object 1 rect from graph 0,graph 0 to graph 1,graph 1 back
set object 1 fc rgb '$background' fs solid 1.0 behind
unset xtics
unset ytics
set cbtics font "$fopfont"
catend
if $useropalette;then
  echo "set palette defined (0 '$ocolour1', 1 '$ocolour2')" >> $output
else
  echo "set palette defined $orbitpalette" >> $output
fi




if $florb&&[ $fnorbis -eq 1 ];then

cat >> $output <<catend
set cbtics font "$pfont"

$cforblabel
plot '<sed "/^#/d" $fsurdat| sort -k4' u $psvar palette
unset multiplot
reset
unset key
catend

else

or=0
while [ $or -lt $fnorbis ];do
orbitaltitel=`grep $or"ORBITAL=" $fsurdat|cut -d'=' -f2`
echo "set label 1 at graph 0.5,1.1 '{/:Bold $orbitaltitel}' font '$fopfont'" >> $output
if $florb;then
  if [ `sed '/#/d' $fsurdat|grep -cE '[0-9]'` -gt 0 ];then
    echo  "plot '$fsurdat' $orbitvarps1:$((4 + $or)) notitle $orbitvarps2 palette" >> $output
  fi
else
  if [ `sed '/#/d' $fsurdat|grep -cE '[0-9]'` -gt 0 ];then
    echo "plot '$fsurdat' $orbitvarps1 notitle $orbitvarps2 lc rgb '$ocolour2'" >> $output
  fi
fi

echo "" >> $output
let or=$or+1
done

fi

echo "unset multiplot" >> $output

fi
fi

fi

fi	

gnuplot $output

rem=`sed -n 32p "../temp$inpar"|grep -Eic "*true*"`
if [ $rem -eq 0 ];then
rm -f $tm1 $tm2 $output
fi

if [ $totspin -eq 2 -a "$spin" = ".spin2" ]||[ $totspin -eq 1 ];then
echo -en "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"

echo "   > Plots created sucessfully <"
fi