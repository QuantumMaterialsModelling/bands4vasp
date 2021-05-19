#!/bin/bash
#$1 folder
#$2 number of equidistance steps
#$3 multifilenames
folder="$1"
fname="$2"


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


while [ -z "$fname" ];do
  read -p "please enter the name of the folders: " fname
  echo ""
done



nline=$(echo $flag|cut -d':' -f1)

let i=nline-1
sed -n 1,$i'p' "$folder/KPOINTS" > "$emptyk"
let i=nline+1
k=0
while true;do
 newline=`sed -n $i'p' "$folder/KPOINTS"`
 isline=$(echo "$newline"|grep -Ec '[0-9]')
 if [ $isline -eq 0 ];then break;fi
 if [ $k -gt 0 ];then
   newf="$k$fname"
   cp -r "$folder" "$newf"
   cp -f "$emptyk" "$newf/KPOINTS"
   echo "$oldline" >> "$newf/KPOINTS"
   echo "$newline" >> "$newf/KPOINTS"

#edit the next 2 lines to change the jobID for each jobfile
#  jobname=`grep 'JOB_ID=' "$newf/jobEBS"|cut -d"'" -f2`
#  sed -i "s/JOB_ID='$jobname'/JOB_ID='$jobname$i'/" "$newf/jobEBS"

   echo "*********** prepared $newf **************"
   echo "$oldline >> $newline"
   echo ""
 fi
 oldline="$newline"
 let k++
 let i++
done


rm "$emptyk"
