# Manual for the bands4vasp post-processing package


bands4vasp -- post processing package for the analysis of unfolded eigenstates in VASP, and much more: band structures, 2D and 3D Fermi surfaces, Fermi vectors and spectral functions.
***
  
  
## About bands4vasp


The **bands4vasp** post-processing package is exclusively build for the analysis and visualisation of bandstructure and especially unfolding calculations from VASP.
It uses the energy values, the k-space coordinates and optionally the orbital and Bloch characters from the **PROCAR**, **PROCAR.prim** or **PRJCAR** files.
The reciprocal lattice of the primitive cell, given in the **PRJCAR** file, is the prefered lattice.
If the **PRJCAR** file is not present, the lattice in the **POSCAR** file will be taken.
For a correct bandstructure of an unfolding calculation it is necessary to have at least one **PRJCAR** file in your dataset.
All energy values will be represented with respect to the Fermi energy, where the Fermi energy is taken from the **OUTCAR** file of the self-consistent calculation of the structure,
from each band respectively or by setting the Fermi energy directly.
**bands4vasp** calculates the Fermi vectors for all kind of bands.
With a dataset of line calculations in a surface of the Brillouin zone, **bands4vasp** can calculate the Fermi vectors and show them on that surface.
**bands4vasp** supports also 3 different pre-processing methods for the sampling of the Fermisurface calculations with VASP.
**bands4vasp** is written in _FORTRAN_, it uses _Gnuplot_ for the visualisation and a _Bash-environment_ brings all together.
***
  
  
## In- and output files



### VASP-file (input)



There are 3 types of VASP output files bands4vasp is able to read:

* The **PRJCAR** file is present if the unfolding procedure in VASP is activated and therefore every energy state can be associated with a so called Bloch character.

* The **PROCAR** file comes from calculations with [LORBIT](https://www.vasp.at/wiki/index.php/LORBIT) and contains all information about the orbitals.

* The **PROCAR.prim** file containes both, the Bloch- and the orbital character.

### bands4vasp output files


bands4vasp provides a lot of different information, which are derived from VASP files. For a better overview of the parameters which are used for the calculation, bands4vasp prints them in the _OUTPAR_ file in the execution directory.

#### Datafiles:

The datafiles are all stored in './bands4vasp_data/', except the FERMIROOTS.dat file(s).

* _FERMIROOTS.dat_ contains all information of the evaluated Fermi vectors and some calculation specific information. The file is stored in the execution directory.
* _banddata.dat_ contains the raw data of the whole bandstructure.
* _banddata.slim.dat_ contains the manipulated data of the bandstructure.
* _banddata.specfun.dat_ contains all information of the spectral function from the bandstructure
* _FERMISURFACE.dat_ contains all information of the evaluated fermisurface.
* _FERMISURFACE.specfun_ contains all information of the evaluated fermisurface spectral function.
* _autognuplot_bands4vasp.gnu_ is the gnuplot file, which creates all the plots.

#### Plots

The plots are all stored in './bands4vasp_img/'.

* _Bandsturcture*_ shows the bands inbetween the energy interval defined by _EDELTA1_.
* _EBSbloch*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _EBSorbit*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to orbital character and if present a variable pointsize proportional to the Bloch character.
* _.spec._ shows the spectral function inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _Fermisurface.specfun_ shows the Fermisurface derived from the spectral function.
* _Bandindexplot_ shows the Bandstructure with bandindices occuring in the VASP files.
* _Fermisurface_ shows the derived fermisurface from the Fermi vectors.
* _Fermisurface_bloch_ shows the derived fermisurface from the Fermi vectors with respect to Bloch character.
* _Fermisurface_orbital_ shows the derived fermisurface from the Fermi vecotrs with respect to orbital character.

**N.B.:** For spin polarized calculations the extansion .spin1. and .spin2. are added to the filenames.  

*These plots have also a processed version inbetween the energy interval defined by _EDELTA2_, which is indicated by the extension .processed.  
 
***
  
  

### INPAR - The bands4vasp input file

**bands4vasp** can be controlled by a variety of parameters, this parameters need to be stored in a file called **INPAR**.
If you run bands4vasp and no INPAR file is present in the execution directory, bands4vasp will take the default values. The INPAR file with the default values is stored in the source directory of bands4vasp.
Some parameters have only an effect, if a specific VASP filetype was chosen.

  
#### General control parameters
  
* **EDELTA1** - energy interval for the raw data plots EBSbloch, EBSorbit, EBSbloch.spec and Bandindexplot. If one value is given, the interval will be symmetric around the Fermi level [-EDELTA1;EDELTA1], or one can set the two values individually seperated by a blank.
  
* **EDELTA2** - energy interval for the processed plots, with the same functionality as EDELTA1, with the different that only energy states in this interval are taken account for calculating the Fermi vectors.
  
* **EDIF**  - Energy diffusion from the unfolding calculation. EDIF defines the maximal energy difference for one k-point, inbetween this interval all energy states will merged and represented in the processed data as one state.
  
* **BAVERAGE** - If BAVERAGE is set to .TRUE. the weighted average of the Bloch character is calculated and represented in the processed data, else the values will summed up.
  
* **OAVERAGE** - If OAVERAGE is set to .TRUE. the weighted average of the orbital character is calculated and represented in the processed data , else the values were summed up.
  
* **BLOCH_TRESHOLD** - sets the minimal Bloch character value. Energy states with a Bloch character less than BLOCH_THRESHOLD will rejected. For unfolding calculations it is recommended to set it to a smal value.
  
* **EFERMI** - sets the source for the Fermi energy. Default is from ./OUTCAR. It is possible to give a directory to the OUTCAR file (e.g. /home/calculation/), to set the Fermi energy explicitly (e.g. 0.4), or by setting the keyword 'bands' to take the Fermi energy from the OUTCAR files of each banddata respectively.
  
* **EGAP** - defines the maximal energy difference of energy states from proximate k-points. Proximate energy states with a energy difference less than this value are considered to belong to the same band. This is used for the Fermi vector calculation.
  
* **DBLOCH** - defines the maximal Bloch character difference for proximate energy states to belong to the same band. This is used for the Fermi vector calculation.
  
* **ODISTINCT** - defines the maximal orbital character difference for proximate energy states of each orbital to belong to the same band. This is used for the Fermi vector calculation.
  
* **GRADIENTD** - sets the maximal gradient deviation.
  
* **NPOINTS** - defines the number of energy states above and below the fermi level, which will be included to the Fermi vector calculation.
  
* **LPOLY** - If LPOLY is set to .TRUE. a polynomial interpolation with the degree 2*NPOINTS will be used for calculating the Fermi vector. If it is set to .FALSE. (default), the coordinates of the Fermi vectors are calculated by linear regression.
  
* **REGULAPREC** - sets the accuracy for the Regular falsi method, which is used for the polynomial interpolation.
  
* **PLOTORB** - sets the orbital number for the orbitalplots in order of appearence in PROCAR[.prim] file. If it's set to 0 (default) all orbitals will be considerd and visualized in EBSorbit_ALL. For any negative value bands4vasp calculates the total amount of all orbitals (EBSorbit_tot).
  
* **BNDDIFF** - IF BNDDIFF is set to .TRUE. (default) bands4vasp considers bandcrossing from bands with different bandindices for the Fermi vector calculations. If set to .FALSE., only bands with the same bandindex are considered as one band. In most of the cases it is the best to set it .TRUE. for unfolded bands and .FALSE. for non-unfolded bands, but one can always take a look at the _Bandindexplot_ to be sure.
  
* **KAPPANORM** - If KAPPANORM is set to .TRUE. the Bloch character for each band and K-point from the super cell will be normalized respectively. This works only with **PRJCAR** files.
  
* **SELECTION** - select a specific Ion and the associated orbital characters in order of appearence in PROCAR[.prim] file. By giving two numbers seperated by a minus 'n-m', bands4vasp will take ion n up to m and sum the orbital characters together. If the value is 0 (default) all ions are considered.
  
* **SKIPKPOINT** - if SKIPKPOINT is set to one integer n, the first n k-points will be ignored. By setting two numbers seperated by a minus 'n-m', bands4vasp will ignore k-points n up to m.
  
* **ROOTSCALC** - if ROOTSCALC is set to .FALSE. no Fermi vectors will be calculated.

  
#### Fermi surface

* **SYMPOINT1** - needs to be initialized as a 3-dimensional vector. The vector elements are seperated by blanks (e.g. SYMPOINT1=0.0 0.0 0.0). If SYMPOINT1 is set, it will be taken as the center of point reflection for the Fermi surface.
  
* **SYMPOINT2** - needs to be initialized as a 3-dimensional vector. The vector elements are seperated by blanks (e.g. SYMPOINT2=0.0 0.0 0.0). If SYMPOINT1 and SYMPOINT2 are set, bands4vasp will do a axis reflection on the Fermi surface with respect to the axis defined by the 2 SYMPOINTS. The points will only be accept, if they are on the Fermi surface.
  
* **SYMREC** - if SYMREC is set to .TRUE., the coordinates for SYMPOINT1 are treated as reciprocal coordinates (default). If SYMREC is set to .FALSE., bands4vasp consideres cartesion coordinates.
  
* **FSURCART** - if FSURCART is set to .TRUE., the cartesian distances of the surface are shown in the plots, else it will show the k-vectors (default).
  
* **FGRID** - An integer value gives the density of a grid for the Fermi surface plots. If set to 0 (default) no grid will be shown.
  
  
#### Spectral function

* **SPECFUN** - if SPECFUN is set to .TRUE., the spectral function of the bandstructure/fermisurface is calculated and visualized (default = .FALSE..).
  
* **SIGMA** - sets the smearing of the deltafunction, which is used for the spectral function.
  
* **SPECDELTA** - The smaller this value (in eV), the higher is the number of energy states of a k-point included in the evaluation of the spectral function. Hence a smaller value results in a higher resolution, more computing time and larger *.specfun.* files.
  
* **SLIMSPEC** - if SLIMSPEC is set to .TRUE., only data with a Bloch character greater than 0 will be written and no interpolation will be done.
  
  
#### Plot specific options

* **MAKEPLOTS** - if MAKEPLOTS is set to .FALSE. no plots will be created.
  
* **FILEFORMAT** - bands4vasp supports a choice of different file format of the plots. Possible formats are: png, pngcairo, pdf, eps (default).
  
* **BANDINDEXPLOT** - if set to .TRUE. the Bandindexplot will be created.
  
* **LEAVEPLOTDATA** - if set to .FALSE. the directory './bands4vasp_data/' will be removed after bands4vasp is finished.
  
  
#### Visual parameters

* **PATHPOINTS** - sets the letters for each pathpoint seperated by a blank. A '/' infrot of a letter print the greek letter. For example _PATHPOINTS=/G M X Y_
  
* **LFITPOINTS** - if set to .TRUE. the fitpoints from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the processed versions of the plots.
  
* **LLINES** - if set to .TRUE. the graph from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the processed versions of the plots.
  
* **LROOTS** - if set to .TRUE. the Fermi roots from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the processed versions of the plots. If there are no Fermi roots found, the energy interval of the band gap is show in the plots, only if LROOTS is not set to .FALSE..
  
* **PSFAC** - is a factor for the general pointsize of all plots. By default it is 1.0.
  
* **BCOLOURS** - is defined by two gnuplot colors seperated by a blank. The colors define the color palette for the Bloch plots.
  
* **OCOLOURS** - same as BCOLOURS, but for the orbital plots.
  
* **BACKCOLOUR** - sets the background color of all plots, except the spectral function plots.

***
  
    
    
    
## Getting started
  
For the installation **bands4vasp_v**< version >**.tar.gz** and **install.sh** are needed. The installation starts with an execution of the install.sh file  
  
> $source ./install.sh  
  
  
    
### Usage
  
  
> b4vasp [OPTION] ... [file]  
  

To start a calculation there have to be the following files:

* **PROCAR.prin**, **PROCAR** or **PRJCAR** :: For the data of the bandstructure.
* **PRJCAR** or **POSCAR** :: For the information of the lattice. When analysing unfolding calculations it is recommended to have at least one **PRJCAR** file.
* **OUTCAR** :: If the Fermi energy is not given in the INPAR file, bands4vasp takes it from **OUTCAR**.

There are 3 different ways of using bands4vasp:  

* **Single path** calculations can be done in the directory of the VASP files by entering the command 'b4vasp', or one can pass a directory
    
> $ b4vasp "directory"  
  
  
* **Multi path** calculations are possible if there are numbered folders including the VASP files from the band calculation. To pass this kind of structure a %-sign represents the number in the foldername.
    
> $ b4vasp %calc
  
>>   This command will pass all directories with numbers from 1-1000 (also with leading zeros) and the name 'calc'  
>>   => 1calc, 01calc, 001calc, 2calc, ....
  
* **Fermisurface** calculations need the same structure as the multi path calculations and additionally with option flag --fermi.
    
> $ b4vasp --fermi %calc
  
   To get a reasonable result, the line calculations performed with VASP need to be on one plane,  
   but the geometrical arrangement of the lines is not important.
  
  
  
#### Options
  
Beside the --fermi option is also the option -rs [--readsave]. The -rs option reads the raw data from the calculation before, which can save a lot of time caused by reading the VASP files. It can be used in the following way:  
  
* First run a calculation in the normal mode.  
> $ b4vasp %calc  
  
* Now one can modify the INPAR file and run the calculation again, but this time without a path specification, with the optional --fermi option and the option -rs. 
> $ b4vasp --fermi -rs  
  
**CAUTION:** There are some parameter, which can not be reset in the --readsave mode. **EDELTA1** and **EDELTA2** can only set to intervals which are included in the original **EDELTA1** interval. **SKIPKPOINT**, **SELECTION**, **EFERMI**, **KAPPANORM** and **BLOCH_THRESHOLD** don't have any influence on the --readsave mode.  
  
There aro also some pre-processing options available. For all the following pre-processing procedures there has to be a folder, which includes all files for the VASP calculation. The section in the KPOINTS file, where one specifies the coordinates of the k-points, is replaced by the flag '#makepath', followed by coordinates depending on the type of sampling. The entries above the '#makepath' flag are copied in every KPOINTS file followed by the calcuclated coordinates. A KPOINTS file for a radial sampling with the first line from center (0, 0, 0) to (0.5, 0, 0) and the last line from (0, 0, 0) to (0, 0.5, 0) in reciprocal coordinates is shown below.  
  
  
> #################### **KPOINTS** ####################  
> _automatic radial sampling_  
> _100_  
> _p_  
> _line_  
> _rec_  
> _#makepath_  
> _0 0 0_  
> _0.5 0 0_  
> _0 0.5 0_  
> ##################################################  
  
More information about [unfolding method with vasp](https://www.vasp.at/wiki/index.php/LKPROJ) or the [KPOINTS](https://www.vasp.at/wiki/index.php/KPOINTS) file can be found on the [vasp wiki page](https://www.vasp.at/wiki/index.php/The_VASP_Manual). There are 3 typs of sampling methods:    
  
* --pre-lines $1 $2 :: prepares a structure of line calculations for each pair of k-points in KPOINTS file after the flag '#makepath'.
                       The following two informations need to be passed:  
                       $1 => The directory for the prepared VASP files  
                       $2 => The name for the new created multi-directory  
  
* --pre-circles $1 $2 $3 :: prepares radial centered sampling. The specification of the radial sampling is done by three coordinates defined in the KPOINTS file, after the flag '#makepath':  
                            The **first** coordinate is the center of the circle.   
                            The **second** coordinate defines the first line of the sampling, starting from the center.  
                            The **third** coordinate defines the last line of the sampling.  
                            The sampling is done in a mathematical positiv sense. The following 3 informations need to be passed:  
                            $1 => directory for the prepared VASP files  
                            $2 => number of equidistant points on the circle  
                            $3 => name of the new created multidirectory   
  
* --pre-surface $1 $2 $3 :: prepares a equidistant sampling within a rectangle defined by the coordinates given in the KPOINTS file after the flag '#makepath':  
                            The **first** coordinate is the corner of the rectangle where the calculation starts.  
                            The **second** coordinate defines with the first one, the direction and the length of the line calculations.  
                            With the **third** coordinate the rectangle is defined and as well the translation direction of the line sampling.  
                            The following 3 informations need to be passed:  
                            $1 => directory for the prepared VASP files  
                            $2 => number of equidistant lines on the surface  
                            $3 => name for the multidirectory  
***  
  
> There are also the options --help and --info available, which prints the most inportant informations about bands4vasp.