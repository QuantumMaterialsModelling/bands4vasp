#!/bin/bash
# $1=folder with VASP files
# $2=number of calculation steps

kcoord="band4vasp_KPOINTScoordinates"
emptyk="band4vasp_emptyKPOINTS"
circdat="fermicircle4VASP.dat"

folder="$1"
ntot="$2"
fname="$3"

if [ ! -f "$folder/KPOINTS" ];then
  echo "ERROR: The directory you passed '$folder'"
  echo "       doesn't contain a KPOINTS file"
  exit
fi

flag=$(grep -in '#makepath' "$folder/KPOINTS")
if [ -z "$flag" ];then
  echo "ERROR: Couldn't found the '#makepath' flag in the KPOINTS file:"
  echo "       $folder/KPOINTS"
  echo "       For more information see --info or the documentation"
  exit
fi

while [ $(echo $ntot|grep -Ec '^[0-9]+$') -eq 0 ];do
  read -p "please enter the number of equidistance steps: " ntot
  echo ""
done

if [ -z "$fname" ];then
  echo "please enter the name of the new folders"
  echo "or you can press enter to name it 'radial':"
  read fname
  if [ -z "$fname" ];then fname="radial";fi
fi

nline=$(echo $flag|cut -d':' -f1)

let i=nline-1
sed -n 1,$i'p' "$folder/KPOINTS" > "$emptyk"
let i=nline+1

filelines=$(cat "$folder/KPOINTS"|wc -l)
nci=0

rm -f "$kcoord"
while [ $i -le $filelines ];do
  
  tempcoord="$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $1" "$2" "$3}')"
  if [ -z "$(echo $tempcoord)" ]||[ $(echo "$tempcoord"|grep -cE '^#') -eq 1 ];then
    let i++
    continue
  fi
  j=1
  while [ $j -le 3 ];do
     if [ $(echo "$tempcoord"|cut -d' ' -f$j|grep -Ec '^[-]{0,}[0-9]') -eq 0 ];then
	echo "ERROR: The coordinate in line $i is not 3-dimensional!!!"
	exit
     fi
     let j++
  done
  echo "$tempcoord" >> "$kcoord"
  let nci++
  if [ $nci -eq 1 ];then sed -n $i'p' "$folder/KPOINTS" >> "$emptyk" ;fi
  if [ $nci -eq 2 ];then w1=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}');fi
  if [ $nci -eq 3 ];then w2=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}');fi
  let i++
done

if [ $nci -ne 3 ];then
  echo "ERROR: There are not 3 coordinates to define the radial sampling."
  echo "       For more information enter 'b4vasp --info' or see the documantation"
  exit
fi


nw=0
if [ `echo $w1|grep -Ec '[0-9]'` -eq 1 ];then
  nw=1
  if [ `echo $w2|grep -Ec '[0-9]'` -eq 0 ];then
     w2="$w1"
  fi

fi

getradlines4vasp "$ntot" "$kcoord" "$circdat" "$nw" "$w1" "$w2"



i=1

while [ $i -le $ntot ];do

newf=$i"$fname"

  if [ -d "$newf" ];then
     echo "WARNING: The directory '$newf' already exits."
     echo "         Do you want to replace it? [y/n]"
     read answ
     if [ $(echo $answ|grep -ciE '^n') -eq 1 ];then
       echo "skipped '$newf'"
       let i++
       continue
     else
       rm -r "$newf"
     fi
  fi



cord2=$(sed -n $i'p' "$circdat")
  cp -r $folder $newf
  cp "$emptyk" $newf/KPOINTS
  echo $cord2 >> "$newf/KPOINTS"
  
  #Edit the following 2 lines for changing the job id for every calculation path
#  jobname=`grep 'JOB_ID=' "$newf/jobEBS"|cut -d"'" -f2`
#  sed -i "s/JOB_ID='$jobname'/JOB_ID='$jobname$i'/" "$newf/jobEBS"

  echo "prepared $newf"

  let i++

done
rm "$circdat" "$emptyk" "$kcoord"
