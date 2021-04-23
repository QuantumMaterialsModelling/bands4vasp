# Manual for the bands4vasp post-processing package


bands4vasp -- post processing package for the analysis of unfolded eigenstates in VASP, and much more: band structures, 2D and 3D Fermi surfaces, Fermi vectors and spectral functions.
***
  
  
## About bands4vasp


The **bands4vasp** post-processing package is exclusively build for the analysis and visualisation of bandstructure- and especially unfolding calculations from VASP.
It uses the energy values, the k-space coordinates and optionally the orbital- and Bloch characters from the **PROCAR**, **PROCAR.prim** or **PRJCAR** files.
Also a lattice is needed to project the k-points on the Fermi level and calculate the Fermi vectors.
The reciprocal lattice of the primitive cell, given in the **PRJCAR** file, is the prefered lattice.
If the **PRJCAR** file is not present, the lattice given in the **POSCAR** file will be taken.
For a correct bandstructure of an unfolding calculation it is necessary to have at least one **PRJCAR** file in your dataset.
All energy values will be represented with respect to the fermi-energy, where the Fermi energy is taken ether from the **OUTCAR** file of the self-consistent calculation of the structure,
from each band respectively or by setting the Fermi energy directly.
**bands4vasp** calculates the roots at the fermi level for all kind of bands. All this information are writen in specialized files and visualized in several plots.
With a dataset of line calculations in a surface of the Brillouin zone, **bands4vasp** can calculate the fermiroots and project it onto that surface.
**bands4vasp** supports also 3 different pre-processing methods for the sampling of the Fermisurface calculations with VASP.
**bands4vasp** is written in _FORTRAN_, it uses _Gnuplot_ for the visualisation and a _Bash-environment_ which brings all together.
***
  
  
## In- and output files



### VASP-file (input)



There are 3 typs of VASP output files bands4vasp is able to read:

* The **PRJCAR** file is present if the unfolding procedure in VASP is activated and therefore every energy value can be associated with a so called Bloch character.

* The **PROCAR** file comes from calculations with LORBIT (see vaspwiki) and contains all information about the orbitals.

* The **PROCAR.prim** file containes both, the Bloch- and the orbital character.

### bands4vasp output files


bands4vasp provides a lot of different information, which are derived from VASP files.

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
* _Fermisurface_bloch_ shows the derived fermisurface from the Fermi vectors with respect to Bloch character.
* _Fermisurface_orbital_ shows the derived fermisurface from the Fermi vecotrs with respect to orbital character.

**N.B.:** For spin polarized calculations the extansion .spin1. and .spin2. are added to the filenames.  

*These plots have also a manipulated version inbetween the energy interval defined by _EDELTA2_, which is indicated by the extension .manipulated.  
 
***
  
  

### INPAR - The bands4vasp input file

**bands4vasp** can be controlled by a variety of parameters, this parameters need to be stored in a file called **INPAR**.
If you run bands4vasp and no INPAR file is present in the execution directory, bands4vasp will take the default values. The INPAR file with the default values is stored in the source directory of bands4vasp.
Some parameters have only an effect, if a specific VASP filetype was choosen, because not every information is stored in all of the 3 filetyps.

  
#### General control parameters
  
* **EDELTA1** - energy interval for the raw data plots EBSbloch, EBSorbit, EBSbloch.spec and Bandindexplot. If one value is given, the interval will be symmetric around the fermi level [-EDELTA1;EDELTA1], or one can set the two values individually seperated by a blank.
  
* **EDELTA2** - energy interval for the manipulated plots, with the same functionality as EDELTA1.
  
* **EDIF**  - Energy diffusion from the unfolding calculation. EDIF defines the maximal energy difference for one k-point, inbetween this interval all energy states will merged and represented in the manipulated data as one state.
  
* **BAVERAGE** - If BAVERAGE is set to .TRUE. the weighted average of the Bloch character is calculated and represented in the manipulated data, else the values will summed up.
  
* **OAVERAGE** - If OAVERAGE is set to .TRUE. the weighted average of the orbital character is calculated and represented in the manipulated data , else the values were summed up.
  
* **BLOCH_TRESHOLD** - sets the minimal Bloch character value. Energy states with a Bloch character less than BLOCH_THRESHOLD will rejected.
  
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

* **SYMPOINT1** - needs to be initialized as a 3-dimensional vector, the vector elements are seperated by blanks (e.g. SYMPOINTS1=0.0 0.0 0.0). If SYMPOINT1 is set, it will be taken as the center of point reflection for the Fermi surface.
  
* **SYMREC** - if SYMREC is set to .TRUE., the coordinates for SYMPOINT1 are treated as reciprocal coordinates (default). If SYMREC is set to .FALSE., bands4vasp consideres cartesion coordinates.
  
* **FGRID** - An integer value gives the density of a grid for the Fermi surface plots. If set to 0 (default) no grid will be shown.
  
  
#### Spectral function

* **SPECFUN** - if SPECFUN is set to .TRUE., the spectral function of the bandstructure/fermisurface is calculated and visualized (default = .FALSE..).
  
* **SIGMA** - sets the smearing of the deltafunction, which is used for the spectral function.
  
* **SPECDELTA** - The smaller this value (in eV), the higher is the number of energy states of a k-point included in the evaluation of the spectral function. Hence a smaller value results in a higher resolution, more computing time and larger *.specfun.* files.
  
* **SLIMSPEC** - if SLIMSPEC is set to .TRUE., only data with a Bloch character greater than 0 will be written and no interpolation will be done.
  
  
#### Plot specific options

* **MAKEPLOTS** - if MAKEPLOTS is set to .FALSE. no plots will be created.
  
* **FILEFORMAT** - bands4vasp supports a choice of different file format of the plots. Possible formats are: png, pngcairo, eps (default).
  
* **BANDINDEXPLOT** - if set to .TRUE. the Bandindexplot will be created.
  
* **LEAVEPLOTDATA** - if set to .FALSE. the directory './bands4vasp_data/' will be removed after bands4vasp is finished.
  
  
#### Visual parameters

* **PATHPOINTS** - sets the letters for each pathpoint seperated by a blank. A '/' infrot of a letter print the greek letter. For example _PATHPOINTS=/G M X Y_
  
* **LFITPOINTS** - if set to .TRUE. the fitpoints from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the manipulated versions of the plots.
  
* **LLINES** - if set to .TRUE. the graph from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the manipulated versions of the plots.
  
* **LROOTS** - if set to .TRUE. the Fermi roots from the linear regression/polynomial interpolation are shown in the plots. By default they are only shown in the manipulated versions of the plots.
  
* **PSFAC** - is a factor for the general pointsize of all plots. By default it is 1.0.
  
* **BCOLOURS** - is defined by two gnuplot colors seperated by a blank. The colors define the color palette for the Bloch plots.
  
* **OCOLOURS** - same as BCOLOURS, but for the orbital plots.
  
* **BACKCOLOUR** - sets the background color of all plots, except the spectral function plots.

***
  
    
    
    
## Getting started
  
For the installation _bands4vasp_v1.0.tar.gz_ and _install.sh_ are needed. The installation starts with an execution of the install.sh file  
  
_=> ./install.sh_  
  
  
    
### Usage
  
  
b4vasp [OPTION] ... [file]  
  

To start a calculation there have to be the following files:

* **PROCAR.prin**, **PROCAR** or **PRJCAR** :: For the data of the bandstructure.
* **PRJCAR** or **POSCAR** :: For the information of the lattice. When analysing unfolding calculations it is recommended to have at least one **PRJCAR** file.
* **OUTCAR** :: If the Fermi energy is not given in the INPAR file, bands4vasp takes it from **OUTCAR**.

There are 3 different ways of using bands4vasp:  

* **Single path** calculations can be done in the directory of the VASP files by entering the command 'b4vasp', or one can pass a directory
    
=> b4vasp "directory"  
  
  
* **Multi path** calculations are possible if there are numbered folders including the VASP files from the band calculation. To pass this kind of structure a %-sign represents the number in the foldername.
    
=> b4vasp %calc
  
   This command will pass all directories with numbers from 1-1000 (also with leading zeros) and the name 'calc'  
   => 1calc, 01calc, 001calc, 2calc, ....
  
* **Fermisurface** calculations need the same structure as the multi path calculations, but also the option --fermi need do be added.
    
=> b4vasp --fermi %calc
  
   To get a reasonable result, the line calculations performed with VASP need to be on one plane,  
   but the geometrical arrangement of the lines is not important.
  
  
  
#### Options
  
Beside the --fermi option are also some pre-processing options. For all the following pre-processing procedures a folder needs to be prepared, which includes all the needed files for the VASP calculation. The section in the KPOINTS file, where one specifies the coordinates of the k-points, is replaced by the flag '#makepath', followed by coordinates depending on the type of sampling:  
  
* --pre-lines $1 $2 :: prepares a structure of line calculations for each pair of k-points in KPOINT file after the flag '#makepath'.
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
  
There are also the options --help and --info available, which prints the most inportant informations about bands4vasp.
