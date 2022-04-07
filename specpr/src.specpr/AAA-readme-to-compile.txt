To compile specpr:

1)  source the environmental variables file for desired system
2)  run make really_clean
3)  run make   (using the supplied makefile for LINUX, Windows, and/or CYGWIN)

Note: Use Cygwin on a Windows-PC (here using ver. 1.7.9.1)
      to make Windows or Cygwin executables.
      the compiler will cross-link using the proper libraries.
---------------------------------------------------------
example:

cd /src/local/specpr/config
source ./cshenv.linux64.gfortran
cd /src/local/specpr/src.specpr
make really_clean
make

---------------------------------------------------------