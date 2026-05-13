#!/bin/bash
# $1 newfilename
# $2 latticevectors filename
# $3 fermienergie filename
# $4 temporary directory


# Removes redundant whitespace from a string
rclean() {
  rcs="$1"
  rcs=$(echo "$rcs" | tr -s ' ' | sed 's/^[ \t]*//;s/[ \t]*$//')
  echo "$rcs"
}

# -------------------------------------------------------------------------------------------
# For a filename='fbegin$fend', this function finds all matching files and saves
# their names in the array 'directory'. The count is stored in $filenumber.
# Example: directory[@] -> fbegin01fend fbegin02fend   filenumber -> 2
# -------------------------------------------------------------------------------------------

checkfile() {
  temp="$1"
  unset directory[@]
  digit="[[:digit:]]{1,}"
  npos=$(echo "$temp" | awk '{print index($0,"%")}')
  if [ "$npos" -eq 0 ]; then
    if [ -f "$temp" ]; then
      filenumber=1
      directory+=("$temp")
      return 1
    else
      filenumber=0
      return 1
    fi
  fi
  filenumber=0
  ndpos=$(echo "$temp" | awk '{print index($0,"/")}')
  if [ "$ndpos" -gt 0 ]; then
    nsep=$(echo "$temp" | grep -o '/' | wc -l)
    nsep=$((nsep + 1))
    nss=$(echo "$temp" | cut -d'/' -f"$nsep" | grep -o '%' | wc -l)
    nameend=$(echo "$temp" | cut -d'/' -f"$nsep")
    lspos=$((${#temp} - ${#nameend}))
    fsbeg=$(echo "$temp" | awk -v pos=$lspos '{print substr($0,1,pos-1)}')
    fsend=$(echo "$temp" | awk -v pos=$lspos '{print substr($0,pos+1)}')
    if [ "$nss" -eq 0 ]; then
      npos=$(echo "$fsbeg" | awk '{print index($0,"%")}')
      fbegin=$(echo "$fsbeg" | awk -v pos=$npos '{print substr($0,1,pos-1)}')
      fend=$(echo "$fsbeg" | awk -v pos=$npos '{print substr($0,pos+1)}')
      npos2=$(echo "$fend" | awk '{print index($0,"%")}')
      if [ "$npos2" -gt 0 ]; then
        fbegin1=$(echo "$fend" | awk -v pos=$npos2 '{print substr($0,1,pos-1)}')
        fend1=$(echo "$fend" | awk -v pos=$npos2 '{print substr($0,pos+1)}')
        hlist=$(find $fbegin*$fbegin1*$fend1/ -name "$fsend" | grep -Ei "$fbegin$digit$fbegin1$digit$fend1")
        nlist1=$(echo "$hlist" | grep -Eo "$fbegin$digit$fbegin1" | grep -oE "$digit" | sort -n | uniq)
        nlist2=$(echo "$hlist" | grep -Eo "$fbegin1$digit$fend1" | grep -oE "$digit" | sort -n | uniq)
        for nl1 in $nlist1; do
          for nl2 in $nlist2; do
            tfile="$fbegin$nl1$fbegin1$nl2$fend1/$fsend"
            if [ -f "$tfile" ]; then
              filenumber=$((filenumber + 1))
              directory+=("$tfile")
            fi
          done
        done
      else
        hlist=$(find $fbegin*$fend/ -name "$fsend" | grep -Ei "$fbegin$digit$fend")
        nlist=$(echo "$hlist" | grep -Eo "$fbegin$digit$fend" | grep -oE "$digit" | sort -n | uniq)
        for nl in $nlist; do
          tfile="$fbegin$nl$fend/$fsend"
          if [ -f "$tfile" ]; then
            filenumber=$((filenumber + 1))
            directory+=("$tfile")
          fi
        done
      fi
    elif [ "$nss" -eq 1 ]; then
      npos=$(echo "$fsbeg" | awk '{print index($0,"%")}')
      if [ "$npos" -eq 0 ]; then
        npos=$(echo "$fsend" | awk '{print index($0,"%")}')
        fbegin=$(echo "$fsend" | awk -v pos=$npos '{print substr($0,1,pos-1)}')
        fend=$(echo "$fsend" | awk -v pos=$npos '{print substr($0,pos+1)}')
        hlist=$(find $fsbeg/ -name "$fbegin*$fend" | grep -Ei "$fbegin$digit$fend")
        nlist=$(echo "$hlist" | grep -Eo "$fbegin$digit$fend" | grep -oE "$digit" | sort -n | uniq)
        for nl in $nlist; do
          tfile="$fsbeg/$fbegin$nl$fend"
          if [ -f "$tfile" ]; then
            filenumber=$((filenumber + 1))
            directory+=("$tfile")
          fi
        done
      else
        fbegin1=$(echo "$fsbeg" | awk -v pos=$npos '{print substr($0,1,pos-1)}')
        fend1=$(echo "$fsbeg" | awk -v pos=$npos '{print substr($0,pos+1)}')
        npos2=$(echo "$fsend" | awk '{print index($0,"%")}')
        fbegin2=$(echo "$fsend" | awk -v pos=$npos2 '{print substr($0,1,pos-1)}')
        fend2=$(echo "$fsend" | awk -v pos=$npos2 '{print substr($0,pos+1)}')
        hlist=$(find $fbegin1*$fend1/ -name "$fbegin2*$fend2" | grep -Ei "$fbegin1$digit$fend1/$fbegin2$digit$fend2")
        nlist1=$(echo "$hlist" | grep -Eo "$fbegin1$digit$fend1" | grep -oE "$digit" | sort -n | uniq)
        nlist2=$(echo "$hlist" | grep -Eo "$fbegin2$digit$fend2" | grep -oE "$digit" | sort -n | uniq)
        for nl1 in $nlist1; do
          for nl2 in $nlist2; do
            tfile="$fbegin1$nl1$fend1/$fbegin2$nl$fend2"
            if [ -f "$tfile" ]; then
              filenumber=$((filenumber + 1))
              directory+=("$tfile")
            fi
          done
        done
      fi
    elif [ "$nss" -eq 2 ]; then
      npos=$(echo "$fsend" | awk '{print index($0,"%")}')
      fbegin1=$(echo "$fsend" | awk -v pos=$npos '{print substr($0,1,pos-1)}')
      fend1=$(echo "$fsend" | awk -v pos=$npos '{print substr($0,pos+1)}')
      npos2=$(echo "$fend1" | awk '{print index($0,"%")}')
      fbegin2=$(echo "$fend1" | awk -v pos=$npos2 '{print substr($0,1,pos-1)}')
      fend2=$(echo "$fend1" | awk -v pos=$npos2 '{print substr($0,pos+1)}')
      hlist=$(find $fsbeg/ -name "$fbegin1*$fbegin2*$fend2" | grep -Ei "$fbegin1$digit$fbegin2$digit$fend2")
      nlist1=$(echo "$hlist" | grep -Eo "$fbegin1$digit$fbegin2" | grep -oE "$digit" | sort -n | uniq)
      nlist2=$(echo "$hlist" | grep -Eo "$fbegin2$digit$fend2" | grep -oE "$digit" | sort -n | uniq)
      for nl1 in $nlist1; do
        for nl2 in $nlist2; do
          tfile="$fsbeg/$fbegin1$nl1$fbegin2$nl2$fend2"
          if [ -f "$tfile" ]; then
            filenumber=$((filenumber + 1))
            directory+=("$tfile")
          fi
        done
      done
    fi
  else
    fbegin=$(echo "$temp" | awk -v pos=$npos '{print substr($0,1,pos-1)}')
    fend=$(echo "$temp" | awk -v pos=$npos '{print substr($0,pos+1)}')
    npos2=$(echo "$fend" | awk '{print index($0,"%")}')
    if [ "$npos2" -gt 0 ]; then
      fbegin1=$(echo "$fend" | awk -v pos=$npos2 '{print substr($0,1,pos-1)}')
      fend1=$(echo "$fend" | awk -v pos=$npos2 '{print substr($0,pos+1)}')
      hlist=$(find -name "$fbegin*$fbegin1*$fend1" | grep -Ei "$fbegin$digit$fbegin1$digit$fend1")
      nlist1=$(echo "$hlist" | grep -Eo "$fbegin$digit$fbegin1" | grep -oE "$digit" | sort -n | uniq)
      nlist2=$(echo "$hlist" | grep -Eo "$fbegin1$digit$fend1" | grep -oE "$digit" | sort -n | uniq)
      for nl1 in $nlist1; do
        for nl2 in $nlist2; do
          tfile="$fbegin$nl1$fbegin1$nl2$fend1/$fsend"
          if [ -f "$tfile" ]; then
            filenumber=$((filenumber + 1))
            directory+=("$tfile")
          fi
        done
      done
    else
      hlist=$(find -name "$fbegin*$fend" | grep -Ei "$fbegin$digit$fend")
      nlist=$(echo "$hlist" | grep -Eo "$fbegin$digit$fend" | grep -oE "$digit" | sort -n | uniq)
      for nl in $nlist; do
        tfile="$fbegin$nl$fend"
        if [ -f "$tfile" ]; then
          filenumber=$((filenumber + 1))
          directory+=("$tfile")
        fi
      done
    fi
  fi
}

# Interactively ask user for a file, handle selection and fallback
askuser() {
  sfile="$1"     # e.g. PROCAR.prim
  ftf="$2"       # file for the Fermi energy (optional)
  astat=1        # file status: 0 = file is checked, 1 = not checked
  double=$(echo "$sfile" | grep -c '|')
  while true; do
    if [ $double -eq 0 ]; then
      diropt=$(find -name "*$sfile*" | sort)
    else
      s1file=$(echo "$sfile" | cut -d'|' -f1)
      s2file=$(echo "$sfile" | cut -d'|' -f2)
      diropt=$(find -name "*$s1file*" -or -name "*$s2file*" | sort)
    fi
    ndiropt=0
    if [ -n "$diropt" ]; then
      ndiropt=$(echo "$diropt" | wc -l)
      [ $ndiropt -gt 30 ] && ndiropt=30
      for i in $(seq 1 $ndiropt); do
        echo "$i $(echo "$diropt" | sed -n ${i}p)"
      done
      echo ""
      echo "Enter number for the directory, or"
    fi
    if [ "$sfile" = "PROCAR" ] || [ "$sfile" = "PROCAR.prim" ]; then
      echo "enter directory for PROCAR.prim or PROCAR file, or"
      echo 'enter stra%strb/ for multipath calculation,'
      echo 'where "%" represents any number less than 100000'
      echo 'For example => "stra(0)1strb/" ... "stra99strb/"'
    elif [ "$sfile" = "PR?CAR" ]; then
      echo "enter directory for PROCAR.prim, PROCAR or PRJCAR file, or"
      echo 'enter stra%strb/ for multipath calculation,'
      echo 'where "%" goes from (0)1-99 => "stra(0)1strb/" ... "stra99strb/"'
    elif [ "$sfile" = "PRJCAR" ]; then 
      echo "enter directory for one PRJCAR file of the unfolding calculation"
    elif [ "$sfile" = "OUTCAR" ]; then
      echo "enter directory for the OUTCAR file of the self-consistent calculation"
      echo "or enter directly the Fermi-energy with a dot (e.g. 0.0)"
    elif [ $double -eq 1 ]; then
      if [ "$s1file" = "PRJCAR" ]; then
        echo "enter directory for PRJCAR or POSCAR.prim for the lattice vectors"
      elif [ "$s2file" = "OUTCAR" ]; then
        echo "enter directory for OUTCAR or POSCAR for the lattice vectors"
      fi
    fi
    read -p ":" filename

    if [ -f "$filename" ]; then
      echo "$filename"
      astat=0
    fi

    if echo "$filename" | grep -Eq '^[[:digit:]]+$'; then
      # filename is a number
      if [ "$filename" -gt 0 ] && [ "$filename" -le $ndiropt ]; then
        filename=$(echo "$diropt" | sed -n ${filename}p)
        echo "$filename"
        astat=0
        break
      else
        echo "ERROR: The number has to be between 1-$ndiropt"
      fi
    else
      if [ "$sfile" = "OUTCAR" ]; then
        if echo "$filename" | grep -Eq '^-?[0-9]+\.*[0-9]*$'; then
          echo "$filename" > "$ftf"
          astat=2
        fi
      fi
      break
    fi
  done
  return $astat
}

# Resolves a filename and tries to append wanted file if necessary
getdir() {
  filename="$1"
  wanted="$2"
  lmultifile=$(echo "$filename" | grep -o '%' | wc -l)
  last=$(echo "$filename" | awk -F'/' '{print $NF}')
  if [ "$lmultifile" -gt 0 ]; then
    stat=1
    if [ -n "$last" ]; then
      checkfile "$filename"
      if [ "$filenumber" -eq 0 ]; then
        checkfile "$filename/$wanted"
        if [ "$filenumber" -gt 0 ]; then
          filename="$filename/$wanted"
        else
          return 2
        fi
      fi
    else
      filename="$filename$wanted"
      checkfile "$filename"
      [ "$filenumber" -eq 0 ] && return 2
    fi
  else
    stat=0
    if [ -n "$last" ]; then
      if [ ! -f "$filename" ]; then
        if [ -f "$filename/$wanted" ]; then
          filename="$filename/$wanted"
        else
          return 2
        fi
      fi
    else
      if [ -f "$filename$wanted" ]; then
        filename="$filename$wanted"
      else
        return 2
      fi
    fi
  fi
  echo "$filename"
  return $stat
}


#=================================================================
#-------------------------- Program ------------------------------
#=================================================================

allfnames="$1"
infilename="$2"
fpastat="$3"
tmpdir="$4"

tempfile=$(sed -n 1p "$allfnames")     # File for all needed directories
latt=$(sed -n 8p "$allfnames")         # Filename for lattice vector if PRJCAR is missing
fermitf="$tmpdir/$(sed -n 9p "$allfnames")"

fstat=2        # File status: 2=unclear, 1=got filename, 0=file exists
lprj=false     # true = use the PRJCAR file instead of the PROCAR file

rm -f "$fermitf"

#******************************************************************
# Need k-point coordinates and energy values
# Get the PROCAR* or PRJCAR files
#******************************************************************
filename="$infilename"
if [ -z "$filename" ]; then
   fstat=0
   if [ -f "PROCAR.prim" ]; then
     filename="PROCAR.prim"
     filetype=1
   elif [ -f "PROCAR" ]; then
     filename="PROCAR"
     filetype=2
   elif [ -f "PRJCAR" ]; then
     filename="PRJCAR"
     filetype=3
   else
     echo "Need PROCAR.prim, PROCAR or PRJCAR file for calculation"
     echo ""
     askuser "PR?CAR"  # now the variable $filename is set
     fstat=$?
   fi
fi
if [ $fstat -eq 0 ]; then
  lmultifile=0
  if echo "$filename" | grep -qi 'PRJCAR'; then
    lprj=true
    filetype=3
  fi
else
   filetype=1
   gdout=$(getdir "$filename" "PROCAR.prim")
   lmultifile=$?
   if [ $lmultifile -eq 2 ]; then
     filetype=2
     gdout=$(getdir "$filename" "PROCAR")
     lmultifile=$?
     if [ $lmultifile -eq 2 ]; then
       filetype=3
       gdout=$(getdir "$filename" "PRJCAR")
       lmultifile=$?
       if [ $lmultifile -eq 2 ]; then
         echo "ERROR: Couldn't find PROCAR[.prim] or PRJCAR file in $filename"
         exit 0
       fi
       lprj=true
     fi
   fi
   filename="$gdout"
fi

procar="$filename"
promulti=$lmultifile
if echo "$filename" | grep -qi 'PRJCAR'; then
  lprj=true
  lunfold=0
  filetype=3
fi

if ! $lprj; then
  lunfold=0
  filetype=1
  if [ $promulti -eq 0 ]; then
    if [ "$(grep -c 'Unfold proj.' "$procar")" -eq 0 ]; then
      lunfold=1
      filetype=2
    fi
  else
    checkfile "$procar"
    if [ "$(grep -c 'Unfold proj.' "${directory[0]}")" -eq 0 ]; then
      lunfold=1
      filetype=2
    fi
  fi
fi

# PRJCAR filetype
if [ $filetype -eq 3 ]; then
  if [ $promulti -eq 0 ]; then
    prjcar="$procar"
  else
    checkfile "$procar"
    prjcar="${directory[0]}"
  fi

# PROCAR.prim filetype
elif [ $filetype -eq 1 ]; then
  #----------------- get the PRJCAR file ------------------
  fstat=1
  lmultifile=0
  nsep=$(echo "$procar" | grep -o '/' | wc -l)
  if [ "$nsep" -gt 0 ]; then
    filename=$(rclean "$(echo "$procar" | cut -d'/' -f1-"$nsep")")
  fi
  if [ -z "$filename" ] || [ "$nsep" -eq 0 ]; then
    filename='.'
  fi
  if [ $promulti -eq 0 ]; then
    cf=$(rclean "$(echo "$procar" | awk -F'/' '{$NF="";print $0}')")
    if [ -z "$cf" ]; then filename='.'; fi
    if [ -f "$filename/PRJCAR" ]; then
      filename="$filename/PRJCAR"
      echo "took $filename for lattice information"
      fstat=0
      lprj=true
    else
      if [ -f "$filename/POSCAR.prim" ]; then
        filename="$filename/POSCAR.prim"
        echo "took $filename for calculation"
        fstat=0
        lprj=false
      else
        echo "Need PRJCAR or POSCAR.prim file for calculation"
        askuser "PRJCAR|POSCAR.prim"  # now the variable $filename is set
        fstat=$?
      fi
    fi
  fi
  if [ $fstat -eq 1 ]; then
    lprj=true
    gdout=$(getdir "$filename" "PRJCAR")
    lmultifile=$?
    if [ $lmultifile -eq 2 ]; then
      lprj=false
      gdout=$(getdir "$filename" "POSCAR.prim")
      lmultifile=$?
      if [ $lmultifile -eq 2 ]; then
        echo "ERROR: Couldn't find $filename"
        exit 0
      fi
    fi
  fi
  if [ $lmultifile -eq 1 ]; then
    if $lprj; then
      checkfile "$filename/PRJCAR"
    else
      checkfile "$filename/POSCAR.prim"
    fi
    prjcar="${directory[0]}"
  else
    prjcar="$filename"
  fi
else
  prjcar="$latt"
fi

#----------------- get the OUTCAR file ------------------

if [ "$(echo "$fpastat" | grep -ciE '^bands$')" -eq 1 ]; then
   filename="$fermitf"
elif echo "$fpastat" | grep -Eq '^-?\.[0-9]+$|^-?[0-9]+\.?[0-9]*$'; then
   echo "$fpastat" > "$fermitf"
   if [ $filetype -eq 2 ]; then
     llat=true
     filename=$(rclean "$(echo "$procar" | awk -F'/' '{$NF="";print $0}')")
     if [ -z "$filename" ]; then filename='.'; fi
     if [ $promulti -eq 1 ]; then
       checkfile "$filename/OUTCAR"
       if [ -f "${directory[0]}" ]; then
         filename="${directory[0]}"
       else
         checkfile "$filename/POSCAR"
         if [ -f "${directory[0]}" ]; then
           filename="${directory[0]}"
         else
           llat=false
         fi
       fi
     else
       if [ -f "$filename/OUTCAR" ]; then
         filename="$filename/OUTCAR"
       else
         if [ -f "$filename/POSCAR" ]; then
           filename="$filename/POSCAR"
         else
           llat=false
         fi
       fi
     fi
     if ! $llat; then
       echo "Need OUTCAR or POSCAR file for lattice vectors"
       echo ""
       askuser "OUTCAR|POSCAR"  # now the variable $filename is set
       fstat=$?
       if [ $fstat -eq 1 ]; then
         gdout=$(getdir "$filename" "OUTCAR")
         lmultifile=$?
         if [ $lmultifile -eq 2 ]; then
           gdout=$(getdir "$filename" "POSCAR")
           lmultifile=$?
           if [ $lmultifile -eq 2 ]; then
             echo "ERROR: Couldn't find $filename"
             exit 0
           fi
         fi
         filename="$gdout"
       fi
     fi
   else
     filename="$fermitf"
   fi
elif [ -e "$fpastat" ]; then
  if [ -f "$fpastat" ]; then
     filename="$fpastat"
  else
    last=$(echo "$fpastat" | awk -F'/' '{print $NF}')
    if [ $(echo "$fpastat" | grep -ciE '^\./|^/|^~') -eq 0 ]; then fpastat="./$fpastat"; fi
    if [ -z "$last" ]; then
      filename="${fpastat}OUTCAR"
    else
      filename="$fpastat/OUTCAR"
    fi
    if [ ! -f "$filename" ]; then
      echo "ERROR: Couldn't find OUTCAR file in directory"
      echo "    => $filename"
      exit 0
    fi
  fi
else
  if [ -f "OUTCAR" ]; then
    filename="OUTCAR"
  else
    echo "Need OUTCAR file for the Fermi-energy"
    echo ""
    askuser "OUTCAR" "$fermitf"  # now the variable $filename is set
    fstat=$?
    if [ $fstat -eq 1 ]; then
      gdout=$(getdir "$filename" "OUTCAR")
      lmultifile=$?
      if [ $lmultifile -eq 2 ]; then
        echo "ERROR: Couldn't find $filename"
        exit 0
      fi
      filename="$gdout"
    elif [ $fstat -eq 2 ]; then
      # got Fermi-energy, but need lattice
      if [ $filetype -eq 2 ]; then
        llat=true
        filename=$(rclean "$(echo "$procar" | awk -F'/' '{$NF="";print $0}')")
        if [ -z "$filename" ]; then filename='.'; fi
        if [ $promulti -eq 1 ]; then
          checkfile "$filename/OUTCAR"
          if [ -f "${directory[0]}" ]; then
            filename="${directory[0]}"
          else
            checkfile "$filename/POSCAR"
            if [ -f "${directory[0]}" ]; then
              filename="${directory[0]}"
            else
              llat=false
            fi
          fi
        else
          if [ -f "$filename/OUTCAR" ]; then
            filename="$filename/OUTCAR"
          else
            if [ -f "$filename/POSCAR" ]; then
              filename="$filename/POSCAR"
            else
              llat=false
            fi
          fi
        fi
        if ! $llat; then
          echo "Need OUTCAR or POSCAR file for lattice vectors"
          echo ""
          askuser "OUTCAR|POSCAR"  # now the variable $filename is set
          fstat=$?
          if [ $fstat -eq 1 ]; then
            gdout=$(getdir "$filename" "OUTCAR")
            lmultifile=$?
            if [ $lmultifile -eq 2 ]; then
              gdout=$(getdir "$filename" "POSCAR")
              lmultifile=$?
              if [ $lmultifile -eq 2 ]; then
                echo "ERROR: Couldn't find $filename"
                exit 0
              fi
            fi
            filename="$gdout"
          fi
        fi
      else
        filename="$fermitf"
      fi
    fi
  fi
fi

outcar="$filename"
if [ ! -f "$fermitf" ] && [ "$(echo "$fpastat" | grep -ciE '^bands$')" -eq 0 ]; then
  grep 'E-fermi' "$outcar" | tail -n1 | awk '{print $3}' > "$fermitf"
fi

#**************************************************************
# Write the results in $tempfile
#**************************************************************
lpos=0
if [ $filetype -eq 1 ]; then
  lpos=$(echo "$prjcar" | awk -F'/' '{print $NF}' | grep -Eic "POSCAR")
elif [ $filetype -eq 2 ]; then
  lpos=$(echo "$outcar" | awk -F'/' '{print $NF}' | grep -Eic "POSCAR")
  if [ $lpos -eq 1 ]; then
    prjcar="$outcar"
    outcar="$fermitf"
  else
    grep -A4 ' Lattice vectors:' "$outcar" | cut -d'(' -f2 | \
      sed "2d;s/,//g;s/)//" > "$prjcar"
  fi
fi

# Single file
if [ $promulti -eq 0 ]; then
  echo "#interval 1" > "$tempfile"
  echo "$lpos" >> "$tempfile"
  echo "$outcar" >> "$tempfile"
  echo "$prjcar" >> "$tempfile"
  echo "$procar" >> "$tempfile"
  if [ "$(echo "$fpastat" | grep -ciE '^bands$')" -eq 1 ]; then
    nsep=$(echo "$procar" | grep -o '/' | wc -l)
    filename=$(rclean "$(echo "$procar" | cut -d'/' -f1-"$nsep")")
    if [ -z "$filename" ]; then filename='.'; fi
    bandout="$filename/OUTCAR"
    if [ -f "$bandout" ]; then
      grep 'E-fermi' "$bandout" | tail -n1 | awk '{print $3}' > "$fermitf"
    else
      echo "ERROR: There has to be OUTCAR files in every directory"
      echo "       of the calculations if the option EFERMI=bands is set"
      exit 0
    fi
  fi
# Multi file
else
  checkfile "$procar"
  echo "#interval $filenumber" > "$tempfile"
  echo "$lpos" >> "$tempfile"
  echo "$outcar" >> "$tempfile"
  echo "$prjcar" >> "$tempfile"
  for n in $(seq 0 $(($filenumber - 1))); do
    singledir="${directory[$n]}"
    echo "${directory[$n]}" >> "$tempfile"
    if [ "$(echo "$fpastat" | grep -ciE '^bands$')" -eq 1 ]; then
      nsep=$(echo "$singledir" | grep -o '/' | wc -l)
      filename=$(rclean "$(echo "$singledir" | cut -d'/' -f1-"$nsep")")
      if [ -z "$filename" ]; then filename='.'; fi
      bandout="$filename/OUTCAR"
      if [ -f "$bandout" ]; then
        grep 'E-fermi' "$bandout" | tail -n1 | awk '{print $3}' >> "$fermitf"
      else
        echo "ERROR: There has to be OUTCAR files in every directory"
        echo "       of the calculations if the option EFERMI=bands is set"
        exit 0
      fi
    fi
  done
fi

exit $filetype
