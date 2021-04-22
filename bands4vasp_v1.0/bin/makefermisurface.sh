#!/bin/bash
#$1 folder
#$2 number of equidistance steps
#$3 multifilenames
folder="$1"
num="$2"
fname="$3"


kcoord="band4vasp_KPOINTScoordinates"
emptyk="band4vasp_emptyKPOINTS"



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

while [ $(echo $num|grep -Ec '^[0-9]+$') -eq 0 ];do
  read -p "please enter the number of equidistance steps: " num
  echo ""
done

while [ -z "$fname" ];do
  read -p "please enter the name of the folders: " fname
  echo ""
done




unset x[@]

nline=$(echo $flag|cut -d':' -f1)

let i=nline-1
sed -n 1,$i'p' "$folder/KPOINTS" > "$emptyk"
let i=nline+1
let j=i+2
sed -n $i,$j'p' "$folder/KPOINTS"|awk '{print $1" "$2" "$3}' > "$kcoord"

w1=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}')
let i++
w2=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}')
w3=$(sed -n $j'p' "$folder/KPOINTS"|awk '{print $4}')
nw=false
if [ `echo $w1|grep -Ec '[0-9]'` -eq 1 ];then
  nw=true
  if [ `echo $w2|grep -Ec '[0-9]'` -eq 0 ];then
     w2="$w1"
  fi
  if [ `echo $w3|grep -Ec '[0-9]'` -eq 0 ];then
     w3="$w1"
  fi

fi

echo "read in 3 points of surface"
cat "$kcoord"
echo ""

k=1
while [ $k -le 3 ];do
  fermip=`sed -n $k'p' "$kcoord"`
  x+=($fermip)
  let k++
done
da=`echo "scale=8; 1.0 / ( $num - 1 )"|bc`
a=0.0
i=1
while [ $i -le $num ];do
  unset co1[@]
  unset co2[@]
  newf="$i$fname"
  cp -r "$folder" "$newf"
  cp -f "$emptyk" "$newf/KPOINTS"
  k=0
  while [ $k -lt 3 ];do
    ind2=$((3 + $k))
    ind3=$((6 + $k))
    vec=`echo "scale=8;$a * ( ${x[$ind3]} - ${x[$ind2]} )"|bc`
    co1+=(`echo "scale=8;${x[$k]} + $vec"|bc`)
    co2+=(`echo "scale=8;${x[$ind2]} + $vec"|bc`)
    let k++
  done
  if $nw;then
    iw2=`echo "scale=8;$w2 + $a * ( $w3 - $w2 )"|bc`
    vec=`echo "scale=8;$w1 - $w2"|bc`
    iw1=`echo "scale=8;$iw2 + $vec"|bc`
    echo "${co1[@]} $iw1" >> "$newf/KPOINTS"
    echo "${co2[@]} $iw2" >> "$newf/KPOINTS"
  else
    echo ${co1[@]} >> "$newf/KPOINTS"
    echo ${co2[@]} >> "$newf/KPOINTS"
  fi

  #edit the next 2 lines to change the jobID for each jobfile
#  jobname=`grep 'JOB_ID=' "$newf/jobEBS"|cut -d"'" -f2`
#  sed -i "s/JOB_ID='$jobname'/JOB_ID='$jobname$i'/" "$newf/jobEBS"

  echo "prepared $newf"
  a=`echo "scale=8; $a + $da"|bc`
  let i++
done
rm "$emptyk" "$kcoord"
