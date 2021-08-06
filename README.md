# dssat-csm-os
The DSSAT Cropping System Model

The Decision Support System for Agrotechnology Transfer (DSSAT) Version is a software 
application program that comprises crop simulation models for over 42 crops (as of Version 4.8).

For DSSAT to be functional it is supported by data base management programs for soil, 
weather, and crop management and experimental data, and by utilities and application 
programs. The crop simulation models simulate growth, development and yield as a 
function of the soil-plant-atmosphere dynamics.

DSSAT and its crop simulation models have been used for many applications ranging from 
on-farm and precision management to regional assessments of the impact of climate 
variability and climate change. It has been in use for more than 25 years by researchers, 
educators, consultants, extension agents, growers, and policy and decision makers 
in over 179 countries worldwide.

Read more about DSSAT at http://dssat.net/about

See also: [The DSSAT Crop Modeling Ecosystem](https://dssat.net/wp-content/uploads/2020/03/The-DSSAT-Crop-Modeling-Ecosystem.pdf) and
[The DSSAT cropping system model](https://dssat.net/jones_2003_the_dssat_cropping_system_model).

and: [Non-threatening best practice DSSAT Fortran coding guidelines](https://dssat.net/non-threatening-best-practice-dssat-fortran-coding-guidelines). 


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


## Compiling the code ##

The code is compatible with the CMake utility for generating make files
and setting up projects for a variety of IDEs and compilers. To use this feature, 
first download and install CMake. Then set up a CMake project by pointing to the
source code directory and the build directory.

## Structure of the code ##
    .
    ├── <source files>
    ├── CMakeLists.txt
    ├── distclean.cmake
    ├── README.md
    ├── ...
    ├── cmake
    │   └── Modules
    │       ├── SetCompileFlag.cmake
    │       └── SetFortranFlags.cmake
    ├── build
    │   └── ...
    └── Data
        ├── Genotype
        ├── Pest
        └── StandardData 

        
### CMakeLists.txt ###

This file contains all the configuration needed to set up the project.  
Edit this file to make your own configuration and add new projects. 
Comment/Uncomment any lines pertaining to options you may need. 

### distclean.cmake ###

This is a CMake script that will remove all files and folder that are created after running `make`.  You can run this code in one of two ways:

* Execute `cmake -P distclean.cmake`. (The `-P` option to `cmake` will execute a CMake script)
* Execute `make distclean` after your Makefile has been generated.

You shouldn't need to edit this file.

### README.md ###

This File.

### [...] ###

Inside the main directory you will find all subdirectories and source files for your project. All .for, f90, etc.

### cmake/Modules/ ###

This directory contains CMake scripts that aid in configuring the build system.

###### SetCompileFlag.cmake ######

This file defines a function that will test a set of compiler flags to see which one works and adds that flag to a list of compiler flags.  This is used to set compile flags when you don't know which compiler will be used.

###### SetFortranFlags.cmake ######

This file uses the function from `SetCompilerFlag.cmake` to set the DEBUG, TESTING, and RELEASE compile flags for your build.  You might want to inspect this file and edit the flags to your liking.

### build ###

This folder is created to organize all working files inside it, avoiding messing up your source folder. During compilation and linking, working folders are created automatically inside this folder. Any libraries created end up in `mod/`, as well as compiled Fortran `.mod` files.  The executable will end up in `bin/`.  

### Data ###

This folder contains model-specific data for genotypes pest, standard model data, code files, DSSATPRO files, etc.

## Configuring the build ##

It is usually preferred that you do an out-of-source build.  To do this, create a `build/` directory at the top level of your project and build there.  

    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    
When you do this, temporary CMake files will not be created in your `src/` directory.  

As written, this template will allow you to specify one of three different sets of compiler flags.  The default is RELEASE.  You can change this using to TESTING or DEBUG using

    $ cmake .. -DCMAKE_BUILD_TYPE=DEBUG
    
or

    $ cmake .. -DCMAKE_BUILD_TYPE=TESTING

You can provide all kind of information CMake. Some examples can be find at [[CMake Command-Line Options](https://cmake.org/cmake/help/cmake-2.4.html)].

One usable examples could be:

    $ cmake -G "Unix Makefiles" -DCMAKE_Fortran_COMPILER=ifort ..

In this example we are specifying the fortran compiler and the kind of project we want as result (make file project). 

## How to Cite DSSAT ##

If you are planning to use DSSAT in any reports or publications, please make sure to refer to the version number you used.
The version and sub-version numbers can be found in the top section of your output files, e.g., 4.8.X (replace X with current version).
In addition, please use the following two references for DSSAT and the Cropping System Model. Other related publications can be found
in the Documentation section under DSSAT References and Model References.

Hoogenboom, G., C.H. Porter, K.J. Boote, V. Shelia, P.W. Wilkens, U. Singh, J.W. White, S. Asseng, J.I. Lizaso, L.P. Moreno, W. Pavan, R. Ogoshi, L.A. Hunt, G.Y. Tsuji, and J.W. Jones. 2019. The DSSAT crop modeling ecosystem. In: p.173-216 [K.J. Boote, editor] Advances in Crop Modeling for a Sustainable Agriculture. Burleigh Dodds Science Publishing, Cambridge, United Kingdom (http://dx.doi.org/10.19103/AS.2019.0061.10)

Hoogenboom, G., C.H. Porter, V. Shelia, K.J. Boote, U. Singh, J.W. White, W. Pavan, F.A.A. Oliveira, L.P. Moreno-Cadena, J.I. Lizaso, S. Asseng, D.N.L. Pequeno, B.A. Kimball, P.D. Alderman, K.R. Thorp, M.R. Jones, S.V. Cuadra, M.S. Vianna, F.J. Villalobos, T.B. Ferreira,  J. Koo, L.A. Hunt, and J.W. Jones. 2021. Decision Support System for Agrotechnology Transfer (DSSAT) Version 4.8 (www.DSSAT.net). DSSAT Foundation, Gainesville, Florida, USA.

Jones, J.W., G. Hoogenboom, C.H. Porter, K.J. Boote, W.D. Batchelor, L.A. Hunt,  P.W. Wilkens, U. Singh, A.J. Gijsman, and J.T. Ritchie. 2003. DSSAT Cropping System Model. European Journal of Agronomy 18:235-265.

