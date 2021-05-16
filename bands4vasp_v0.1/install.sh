#!/bin/bash
fpath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
hdir="$( cd && pwd)"
foldername='bands4vasp'
bashrc=$hdir'/.bashrc'
com='b4vasp'



echo "The packages 'gfortran', 'gnuplot' and 'ghostscript' are needed"
echo "do you want to continue installation [press enter]"
read -p "or abort to install missing packages manually [enter exit]:" answ
if [ `echo $answ|grep -ic 'exit'` -eq 1 ];then stop;fi
echo ""


while true; do
  read -p "Enter installation path or press ENTER to install in $hdir/$foldername : " ipath
  if [ -z "$ipath" ]; then
    ipath="$hdir/$foldername"
    break
  else
    ipath=`echo "$ipath"|sed 's/[\/]*$//'`
    if [ `echo "$ipath"|grep -cE '^~'` -eq 1 ];then
      ipath=$hdir'/'`echo "$ipath"|sed 's/^~//'`
    fi
    if [ -d "$ipath" ];then
	ipath="$ipath/$foldername"
	break
    else
      read -p "$ipath doesn't exist, creating directory? [y/n]:" answ
      if [ `echo $answ|grep -oE '^.{1}'|grep -ic 'y'` -eq 1 -o -z "$answ" ];then break;fi
    fi
  fi
done

echo " "


tarfile=`ls $fpath|grep  '.tar.gz' | grep 'bands4vasp_v' | tail -n1`

mkdir -p $ipath
tar xfvz $fpath/$tarfile -C $ipath

echo " "

read -p "Do you want to add 'b4vasp' to your PATH variable [y/n]:" ans
echo ""
if [ `echo $ans|grep -ic 'y'` -eq 1 ];then
echo "adding new PATH ..."
if [ `grep -c '#bands4vasp command' $bashrc` -ge 1 ];then
ed -s $bashrc <<END
/#bands4vasp command/+c
export PATH="$ipath/bin:\$PATH"
.
wq
END

else
   echo "" >> $bashrc
   echo '#bands4vasp command' >> $bashrc
   echo "export PATH=\"$ipath/bin:\$PATH\"" >> $bashrc
fi

fi

echo ""



##############################################################
#======================== compiling =========================#
##############################################################

   echo "... compiling source code ..."
   cd "$ipath/src/"

   #compile all needed files
   gfortran -c math.f90
   gfortran math.f90 -c mylattice.f90
   gfortran math.f90 -c ebs_typs.f90
   gfortran math.f90 ebs_typs.f90 mylattice.f90 -c ebs_methods.f90
   gfortran -g -fcheck=all -Wall math.o mylattice.o ebs_typs.o ebs_methods.o ebs_main.f90 -o nebsfitting
   gfortran getradlines.f90 -o getradlines4vasp
   mv getradlines4vasp "$ipath/bin/."
   mv nebsfitting "$ipath/bin/."
   #-------------------------

   cd -  > /dev/null

#============================================================

echo " "
echo ">>>>>>>>> installation complete <<<<<<<<<<<"
echo " "
echo "enter 'b4vasp --help' for more information"
read -p "Do you want to reload your bash [yes/no]:" ans

if [ `echo $ans|grep -ic 'yes'` -eq 1 ];then bash;fi

