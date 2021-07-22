#!/bin/bash
# $1=file with all created plots
# $2=plots are activated?

outfile="$1"
lplots="$2"
imgdir="$3"

echo " "
echo '----------------------------------------------------------'
if [ $lplots -gt 0 ];then
echo "*********** The following plots were created *************"
echo "**********************************************************"
echo " "
list=`cat $outfile | cut -d '/' -f2`
for l in $list;do
  if [ ! "$l/" == "$imgdir" ];then echo $l;fi
done
echo " "
echo "************   in directory $imgdir  *************"
echo '----------------------------------------------------------'

cat $outfile | cut -d'/' -f3|sed '/^$/d'
else
echo "***************** No plots were created ******************"
echo '----------------------------------------------------------'


fi
echo ""
