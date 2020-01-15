rem Compilation of CAF2014 for use in R
gfortran -O3 -c -fdefault-real-8 model\declare_parameters.f90 model\environment.f90 model\abovegroundres.f90 model\management.f90 model\tree.f90 model\shade.f90 model\coffee.f90 model\belowgroundres.f90 model\soil.f90 model\set_params.f90 model\CAF2014.f90
gfortran -shared -o CAF2014.DLL declare_parameters.o environment.o abovegroundres.o management.o tree.o shade.o coffee.o belowgroundres.o soil.o set_params.o CAF2014.o

del *.o
del *.mod

pause