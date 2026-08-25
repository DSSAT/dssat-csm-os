<p align="center">
<img width="400px" alt="DSSAT" src="https://dssat.net/wp-content/uploads/2014/05/DSSAT-color-update.png">
</p>
<p align="center">
<a href="http://dssat.net">[DSSAT Homepage]</a> | 
<a href="http://dssat.net/about">[About DSSAT]</a> | 
<a href="http://dssat.net/contact-us">[Contact us]</a>
</p>
<hr>
The Decision Support System for Agrotechnology Transfer (DSSAT) Version is a software 
application program that comprises crop simulation models for more than 45 crops. The most recent version is DSSAT v4.8.5 released in December, 2024 
<a href="https://github.com/DSSAT/dssat-csm-os/releases/tag/v4.8.5.0">(Check latest RELEASE here)</a>. 

For DSSAT to be functional, it is 
supported by data base management programs for soil, weather, crop management, and experimental data, and by utilities and application 
programs. The crop simulation models simulate growth and development and predict yield, yield components and many other traits and variables as a 
function of the soil-plant-atmosphere dynamics.

Questions about usage of the DSSAT Crop Modeling Ecosystem <a href="http://dssat.net/contact-us">[contact us]</a>.

Do not know how to use DSSAT? Consider participating in the <a href="https://dssat.net/training/upcoming-workshop/">[upcoming DSSAT training workshop]</a>

Read more about DSSAT at <a href="http://dssat.net/about">[DSSAT Homepage]</a>

## The directory structure ##

DSSAT cropping system model (CSM) design is a modular structure in which components 
separate along scientific discipline lines and are structured to allow easy replacement 
or addition of modules. It has one Soil module, a Crop Template module which can simulate 
different crops by defining species input files, an interface to add individual crop 
models if they have the same design and interface, a weather module, and a module for 
dealing with competition for light and water among the soil, plants, and atmosphere. 
It is also designed for incorporation into various application packages, ranging from 
those that help researchers adapt and test the CSM to those that operate the DSSAT /CSM 
to simulate production over time and space for different purposes.

### Structure of the code ###
    .
    ├── build
    │   └── ...
    ├── cmake
    │   └── Modules
    │       ├── SetCompileFlag.cmake
    │       └── SetFortranFlags.cmake
    ├── Data
    |   ├── Genotype
    |   ├── Pest
    |   └── StandardData 
    ├── <source files>
    ├── CMakeLists.txt
    ├── distclean.cmake
    ├── README.md
    └── ...

## Compiling the code ##

The code is compatible with the CMake utility for generating MakeFile
and setting up projects for a variety of IDEs and compilers. To use this feature, 
first download and install CMake. Then set up a CMake project by pointing to the
source code directory and the build directory.

## Configuring the build ##

It is usually preferred that you do an out-of-source build.  To do this, create a `build/` directory at the top level of your project and build there. This folder is created to organize all working files inside it, avoiding messing up your source folder. During compilation and linking, working folders are created automatically inside this folder. Any libraries created end up in `mod/`, as well as compiled Fortran `.mod` files.  The executable will end up in `bin/`.

    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    
When you do this, temporary CMake files will not be created in your `src/` directory.  

As written, this template will allow you to specify one of three different sets of compiler flags.  The default is DEBUG.  You can change this using to RELEASE or DEBUG using

    $ cmake .. -DCMAKE_BUILD_TYPE=DEBUG
    
or

    $ cmake .. -DCMAKE_BUILD_TYPE=RELEASE

You can provide all kind of information CMake. See more information at [[CMake Tutorial](https://cmake.org/cmake/help/latest/guide/tutorial/index.html)].

One usable examples could be:

    $ cmake -G "Unix Makefiles" -DCMAKE_Fortran_COMPILER=ifort ..

In this example we are specifying the fortran compiler and the kind of project we want as result (make file project). 

### CMakeLists.txt ###

This file contains all the configuration needed to set up the project.  
Edit this file to make your own configuration and add new projects. 
Comment/Uncomment any lines pertaining to options you may need. 

### distclean.cmake ###

This is a CMake script that will remove all files and folder that are created after running `make`.  You can run this code in one of two ways:

* Execute `cmake -P distclean.cmake`. (The `-P` option to `cmake` will execute a CMake script)
* Execute `make distclean` after your Makefile has been generated.

### cmake/Modules/ ###

This directory contains CMake scripts that aid in configuring the build system.

###### SetCompileFlag.cmake ######

This file defines a function that will test a set of compiler flags to see which one works and adds that flag to a list of compiler flags.  This is used to set compile flags when you don't know which compiler will be used.

###### SetFortranFlags.cmake ######

This file uses the function from `SetCompilerFlag.cmake` to set the DEBUG, TESTING, and RELEASE compile flags for your build.  You might want to inspect this file and edit the flags to your liking.

### Data ###

This folder contains model-specific data for genotypes, pest, standard model data, code files, DSSATPRO files, etc.

    .
    ├── Genotype
    ├── Pest
    ├── StandardData
    ├── Data.CDE
    ├── Detail.CDE
    ├── DSSATPRO.v48
    ├── ...
    └── README.md
 
The files in this repository can be combined with the files in the Data repository (https://github.com/DSSAT/dssat-csm-data) to 
replicate the directory structure of the Windows installation of DSSAT v4.8.2 (e.g., with the Genotype directory at the
same level as the Alfalfa and other crops directories and the CDE files in the root directory).

## Best DSSAT coding practices ##
See: [Non-threatening best practice DSSAT Fortran coding guidelines](https://dssat.net/non-threatening-best-practice-dssat-fortran-coding-guidelines). 


## How to Cite DSSAT ##

If you are planning to use DSSAT in any reports or publications, please make sure to refer to the version number you used.
The version and sub-version numbers can be found in the top section of your output files, e.g., 4.8.X (replace X with current version).
In addition, please use the following two references for DSSAT and the Cropping System Model. Other related publications can be found
in the Documentation section under DSSAT References and Model References.

Hoogenboom, G., C.H. Porter, K.J. Boote, V. Shelia, P.W. Wilkens, U. Singh, J.W. White, S. Asseng, J.I. Lizaso, L.P. Moreno, W. Pavan, R. Ogoshi, L.A. Hunt, G.Y. Tsuji, and J.W. Jones. 2019. The DSSAT crop modeling ecosystem. In: p.173-216 [K.J. Boote, editor] Advances in Crop Modeling for a Sustainable Agriculture. Burleigh Dodds Science Publishing, Cambridge, United Kingdom (https://dx.doi.org/10.19103/AS.2019.0061.10).

See also: [The DSSAT crop modeling ecosystem](https://dssat.net/wp-content/uploads/2020/03/The-DSSAT-Crop-Modeling-Ecosystem.pdf)

Hoogenboom, G., C.H. Porter, V. Shelia, K.J. Boote, U. Singh, W. Pavan, F.A.A. Oliveira, L.P. Moreno-Cadena, T.B. Ferreira, J.W. White, J.I. Lizaso, D.N.L. Pequeno, B.A. Kimball, P.D. Alderman, K.R. Thorp, S.V. Cuadra, M.S. Vianna, F.J. Villalobos, W.D. Batchelor, S. Asseng, M.R. Jones, A. Hopf, H.B. Dias, A. Jintrawet, R. Jaikla, E. Memic, L.A. Hunt, and J.W. Jones. 2024. Decision Support System for Agrotechnology Transfer (DSSAT) Version 4.8.5 (www.DSSAT.net). DSSAT Foundation, Gainesville, Florida, USA.

Jones, J.W., G. Hoogenboom, C.H. Porter, K.J. Boote, W.D. Batchelor, L.A. Hunt, P.W. Wilkens, U. Singh, A.J. Gijsman, and J.T. Ritchie. 2003. The DSSAT cropping system model. European Journal of Agronomy 18:235-265 (https://doi.org/10.1016/S1161-0301(02)00107-7).

See also: [The DSSAT cropping system model](https://dssat.net/jones_2003_the_dssat_cropping_system_model).

