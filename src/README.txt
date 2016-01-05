To compile for the time being I'm using:
gfortran -Wall -Wextra -Wconversion -fbounds-check *f90 -o test_main.x \
	 -I/data/tempo2/ggonzale/lib/gfortran/include   \
	 -L/data/tempo2/ggonzale/lib/gfortran/lib -lhdf5_fortran

To avoid run time errors add the following to LD_LIBRARY_PATH:
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/data/tempo2/ggonzale/lib/gfortran/lib

