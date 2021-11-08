# The bands4vasp post-processing package

bands4vasp -- post processing package for the analysis of unfolded eigenstates in VASP, and much more: band structures, 2D and 3D Fermi surfaces, Fermi wave vectors and spectral functions.
***
D. Dirnberger, G. Kresse, C. Franchini, M. Reticcioli,\
University of Vienna (Austria)
***
This Readme file serves as a quick guide: for a complete description please refer to the pdf Manual.

## About bands4vasp

The **bands4vasp** post-processing package is tailored for the analysis and visualisation of band structures, Fermi surfaces and spectral functions obtained from VASP calculations, with special focus on unfolding simulations \[Processing of unfolding calculations requires a patched version of VASP that will be available soon\].\
The package can be used in any _Bash environment_, it is written in _FORTRAN_ and it uses _Gnuplot_ for data visualization.

If you use this code please cite:\
D. Dirnberger, G. Kresse, C. Franchini, M. Reticcioli, "Electronic state unfolding for plane waves: energy bands, Fermi surfaces and spectral functions",  J. Phys. Chem. C 125, 12921–12928 (2021). DOI:10.1021/acs.jpcc.1c02318 (arXiv:2103.09540).

About this version, bands4vasp v0.3 (November 2021):
This is the first beta version of the package. Please report us any bug. Comments and suggestions are also welcome.

***

## Getting started

### Installation

The installation requires the **bands4vasp_v**\<version\>**.tar.gz** and **install.sh** files. The installation is perfromed by executing the install.sh file, by running the bash command from the root folder of bands4vasp:
  
> $ source ./install.sh  
        
### Usage

bands4vasp can be executed by the following bash command

> b4vasp \[OPTIONS\] ... \[file\]

If no option is specified, the execution will result in the calculation of energy band structures and spectral functions, using default parameters (see the Manual for a complete description). 

The bands4vasp package can be used in three different modes:

* **Single path** mode: bands4vasp processes the VASP files in the local directory, or a different folder if the path is specified in the command:
    
> $ b4vasp \[folder-path\]

* **Multi path** mode: bands4vasp processes VASP files contained in multiple folders; this mode is useful for memory demading calculations, typical of large supercell in unfolding simulations (the calculation of the eigenvalue gets split in multiple folder, each performed using different k-points in the reciprocal space); the folders should contain a running integer in the folder name; this mode is activated by the % sign followed by the folder base name as option for the b4vasp command:
    
> $ b4vasp %calc
  
>>   This command will pass all directories with numbers from 1-1000 (also with leading zeros) and the name 'calc'  
>>   => 1calc, 01calc, 001calc, 2calc, ....
  
* **Fermi surface** mode: the VASP files need the same structure as in the multi path calculations; the k-points in the VASP calculations should be contained on one single plane; this mode is activated by the flag --fermi.
    
> $ b4vasp --fermi %calc  
  
  
#### Options

Options for the analysis and visualization of VASP calculations:
* --fermi activates the Fermi surface mode (see above).
* -rs \[--readsave\] reads the raw data obtained from a previous execution of bands4vasp (avoiding to read VASP files, which could be time consuming), thus, it is useful for testing and visualization purposes.

Options for setting up input files used in VASP calculations (complete description available in the manual):
* --pre-lines $1 $2 :: for setting up multi path calculations.
* --pre-circles $1 $2 $3 :: for sampling the reciprocal space with a radial grid.
* --pre-surface $1 $2 $3 :: for sampling the reciprocal space with a rectangular grid.

Miscellaneous options:
* --help and --info that print useful information.

  
## List of input/output Files

### bands4vasp output files

The execution of bands4vasp produces different types of files, as describe below.

#### Datafiles:

* _FERMIROOTS.dat_ contains all information of the evaluated Fermi vectors and some calculation specific information.
* _banddata.dat_ contains the raw data of the whole band structure.
* _banddata.slim.dat_ contains the manipulated data of the band structure.
* _banddata.specfun.dat_ contains all information of the spectral function from the band structure
* _FERMISURFACE.dat_ contains all information of the evaluated Fermi surface.
* _FERMISURFACE.specfun_ contains all information of the evaluated Fermi surface spectral function.
* _autognuplot_bands4vasp.gnu_ is the gnuplot file for generating all images.

The datafiles are all stored in './bands4vasp_data/', except the FERMIROOTS.dat file(s).

#### Images

* _Bandsturcture*_ shows the bands inbetween the energy interval defined by _EDELTA1_.
* _EBSbloch*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _EBSorbit*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to orbital character and if present a variable pointsize proportional to the Bloch character.
* _.spec._ shows the spectral function inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _Fermisurface.specfun_ shows the Fermi surface derived from the spectral function.
* _Bandindexplot_ shows the band structure with bandindices occuring in the VASP files.
* _Fermisurface_ shows the derived Fermi surface from the Fermi vectors.
* _Fermisurface_bloch_ shows the derived Fermi surface from the Fermi vectors with respect to Bloch character.
* _Fermisurface_orbital_ shows the derived Fermi surface from the Fermi vecotrs with respect to orbital character.

For spin polarized calculations the extension .spin1. and .spin2. are added to the filenames.

The extension _.processed._ indicates images including technical details (for example, the interpolation calculated to determine the Fermi wave vectors).

Most relevant images are stored in the root directory, while supplementary images are stored in './bands4vasp_img/'.

***

### VASP files (used as input by bands4vasp)

The bands4vasp package can read the following files from VASP calculations:
* the **PRJCAR** file for unfolding calculations in VASP;
* the **PROCAR** file for _nlm_ and site projected wave function character of each eigenvalue;
* the **PROCAR.prim** file, analogous to PROCAR, but for unfolding calculations;
* the **POSCAR** file for information on the lattice and the structure;
* the **OUTCAR** file for retrieving the value of the Fermi energy (if not specified in the INPAR file).

### INPAR - The bands4vasp input file

The **INPAR** files contains all parameters needed for executing bands4vasp.
Default values are used for variables eventually not specified in the INPAR file (the INPAR file with the default values is stored in the installation directory). The list of variables used in the execution of bands4vasp get printed in the **OUTPAR** file.
A complete description of all variables controlled by the INPAR is provided in the Manual.

***
  
    
    
    

