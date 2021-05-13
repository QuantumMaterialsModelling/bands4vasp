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

while [ -z "$fname" ];do
  read -p "please enter the name of the folders: " fname
done

nline=$(echo $flag|cut -d':' -f1)

let i=nline-1
sed -n 1,$i'p' "$folder/KPOINTS" > "$emptyk"
let i=nline+1
let j=i+2
sed -n $i,$j'p' "$folder/KPOINTS"|awk '{print $1" "$2" "$3}' > "$kcoord"
sed -n $i'p' "$folder/KPOINTS" >> "$emptyk" 
let i++
w1=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}')
w2=$(sed -n $j'p' "$folder/KPOINTS"|awk '{print $4}')
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
