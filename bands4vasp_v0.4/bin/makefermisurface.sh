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

if [ -z "$fname" ];then
  echo "please enter the name of the new folders"
  echo "or you can press enter to name it 'surface':"
  read fname
  if [ -z "$fname" ];then fname="surface";fi
fi



unset x[@]

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
  if [ $nci -eq 1 ];then w1=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}');fi
  if [ $nci -eq 2 ];then w2=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}');fi
  if [ $nci -eq 3 ];then w3=$(sed -n $i'p' "$folder/KPOINTS"|awk '{print $4}');fi
  let i++
done

if [ $nci -ne 3 ];then
  echo "ERROR: There are not 3 coordinates to define the equidistant sampling."
  echo "       For more information enter 'b4vasp --info' or see the documantation"
  exit
fi



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
  if [ -d "$newf" ];then
     echo "WARNING: The directory '$newf' already exits."
     echo "         Do you want to replace it? [y/n]"
     read answ
     if [ $(echo $answ|grep -ciE '^n') -eq 1 ];then
       echo "skipped '$newf'"
       a=`echo "scale=8; $a + $da"|bc`
       let i++
       continue
     else
       rm -r "$newf"
     fi
  fi
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
