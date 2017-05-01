# DSSAT-CSM #

DSSAT cropping system model (CSM) design is a modular structure in which components 
separate along scientific discipline lines and are structured to allow easy replacement 
or addition of modules. It has one Soil module, a Crop Template module which can simulate 
different crops by defining species input files, an interface to add individual crop 
models if they have the same design and interface, a Weather module, and a module for 
dealing with competition for light and water among the soil, plants, and atmosphere. 
It is also designed for incorporation into various application packages, ranging from 
those that help researchers adapt and test the CSM to those that operate the DSSAT /CSM 
to simulate production over time and space for different purposes 
[The DSSAT cropping system model](http://abe.ufl.edu/jjones/ABE_5646/Xtra%20files/The%20DSSAT%20Cropping%20System%20Model.pdf).

## The directory structure ##

.
├── CMakeLists.txt
├── distclean.cmake
├── README.md
├── ...
├── cmake
│   └── Modules
│       ├── SetCompileFlag.cmake
│       └── SetFortranFlags.cmake
├── build
│   ├── CMakeFiles
│   │   └── ...
│   ├── bin
│   └── mod
└── Data
    ├── Documentation
    ├── Cotton
    ├── ... 
    └── Wheat


         
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

### ... ###

Inside the main directory you will find all subdirectories and source files for your project. All .for, f90, etc.

### cmake/Modules/ ###

This directory contains CMake scripts that aid in configuring the build system.

###### SetCompileFlag.cmake ######

This file defines a function that will test a set of compiler flags to see which one works and adds that flag to a list of compiler flags.  This is used to set compile flags when you don't know which compiler will be used.

###### SetFortranFlags.cmake ######

This file uses the function from `SetCompilerFlag.cmake` to set the DEBUG, TESTING, and RELEASE compile flags for your build.  You might want to inspect this file and edit the flags to your liking.

### build/bin/ and build/mod ###

These folders are created after running `make`.  Any libraries created end up in `mod/`, as well as compiled Fortran `.mod` files.  The executable will end up in `bin/`.  

### Data ###

This folder contains documentation and experiment files for different crops.

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
