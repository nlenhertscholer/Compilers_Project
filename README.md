# Compilers_Project
by Nesta Lenhert-Scholer and Bowen Bao

Final Project for the Compilers course in the MPCS at the University of Chicago.

## Build
There should already be an executable named [/bin/ekcc](./bin/) that is compiled for the UChicago Linux Machines. If, for whatever reason, that executable does not work, then you can use the following commands to build from source:
```
mkdir build
cd build
cmake ..
make
```
The executable should then be in the same path as defined above.

## Dependencies
- Bison 3.0.4
- Flex 2.6.0
- LLVM 7.0

This program has been tested and runs/builds with the above dependencies on the UChicago Linux Machines. 
This should also work on MacOS if the above dependencies are met, but there is no guarantee that it works as expected. 
