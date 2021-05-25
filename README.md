# The bands4vasp post-processing package

bands4vasp -- post processing package for the analysis of unfolded eigenstates in VASP, and much more: band structures, 2D and 3D Fermi surfaces, Fermi wave vectors and spectral functions.
***
D. Dirnberger, G. Kresse, C. Franchini, M. Reticcioli,
University of Vienna (Austria)
***
This Readme file is supposed to serve as quick guide: for a complete description please refer to the Manual.
***

## About bands4vasp

The **bands4vasp** post-processing package is tailored for the analysis and visualisation of bandstructures, Fermi surfaces and spectral functions obtained from VASP calculations, with special focus on unfolding simulations.
The package can be used in any _Bash environment_, it is written in _FORTRAN_ and it uses _Gnuplot_ for data visualization.
If you use this code please cite:
Dirnberger, D., Kresse, G., Franchini, C., Reticcioli, M. (2021). Electronic state unfolding for plane waves: energy bands, Fermi surfaces and spectral functions ( http://arxiv.org/abs/2103.09540 ).

About this version, bands4vasp 0.1 (May 2021):
This is the first beta version of the package. Please report us any bug. Comments and suggestions are also welcome.

***

## Getting started

### Installation

The installation requires the **bands4vasp_v**< version >**.tar.gz** and **install.sh** files. The installation is perfromed by executing the instructions in the install.sh file, by running the bash command:
  
> $ source ./install.sh  
        
### Usage

bands4vasp can be executed by the following bash command

> b4vasp \[OPTIONS\] ... \[file\]

If no option is specified, the execution will result in the calculation of energy bandstructures and spectral functions, using default parameters (see the Manual for a complete description). 

The following files (from VASP calculations) are required by bands4vasp:

* **PROCAR.prim**, **PROCAR** or **PRJCAR** :: for retrieving information on eigenvalues and eigenvectors;
* **PRJCAR** or **POSCAR** :: for information on the lattice and the structure (**PRJCAR** file is recommended for unfolding calculations);
* **OUTCAR** :: if the Fermi energy is not specified by the user (via the INPAR file), the value is taken from the **OUTCAR**.

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
* _banddata.dat_ contains the raw data of the whole bandstructure.
* _banddata.slim.dat_ contains the manipulated data of the bandstructure.
* _banddata.specfun.dat_ contains all information of the spectral function from the bandstructure
* _FERMISURFACE.dat_ contains all information of the evaluated fermisurface.
* _FERMISURFACE.specfun_ contains all information of the evaluated fermisurface spectral function.
* _autognuplot_bands4vasp.gnu_ is the gnuplot file for generating all images.

The datafiles are all stored in './bands4vasp_data/', except the FERMIROOTS.dat file(s).

#### Images

* _Bandsturcture*_ shows the bands inbetween the energy interval defined by _EDELTA1_.
* _EBSbloch*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _EBSorbit*_ shows the bands inbetween the energy interval defined by _EDELTA1_ with respect to orbital character and if present a variable pointsize proportional to the Bloch character.
* _.spec._ shows the spectral function inbetween the energy interval defined by _EDELTA1_ with respect to Bloch character.
* _Fermisurface.specfun_ shows the Fermisurface derived from the spectral function.
* _Bandindexplot_ shows the Bandstructure with bandindices occuring in the VASP files.
* _Fermisurface_ shows the derived fermisurface from the Fermi vectors.
* _Fermisurface_bloch_ shows the derived fermisurface from the Fermi vectors with respect to Bloch character.
* _Fermisurface_orbital_ shows the derived fermisurface from the Fermi vecotrs with respect to orbital character.

For spin polarized calculations the extansion .spin1. and .spin2. are added to the filenames.

The extension _.processed._ indicates images including technical details (for example, the interpolation calculated to determine the Fermi wave vectors).

Most relevant images are stored in the root directory, while supplementary images are stored in './bands4vasp_img/'.

***

### VASP files (used as input by bands4vasp)

The bands4vasp package can read the following files from VASP calculations:
* the **PRJCAR** file for unfolding calculations in VASP;
* the **PROCAR** file for _nlm_ and site projected wave function character of each eigenvalue;
* the **PROCAR.prim** file, analogous to PROCAR, but for unfolding calculations.

### INPAR - The bands4vasp input file

The **INPAR** files contains all parameters needed for executing bands4vasp.
Default values are used for variables eventually not specified in the INPAR file (the INPAR file with the default values is stored in the source directory of bands4vasp). The list of variables used in the execution of bands4vasp are stored in the **OUTPAR** file.
A complete description of all variables controlled by the INPAR is provided in hte Manual.

***
  
    
    
    

