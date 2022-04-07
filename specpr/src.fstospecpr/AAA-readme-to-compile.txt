To compile fstospecpr:

1)  source the environmental variables file for desired system
2)  run make clean
3)  run make   (using the supplied makefile for LINUX, Windows, and/or CYGWIN)
4)      make install

Note: Use Cygwin on a Windows-PC (here using ver. 1.7.9.1)
      to make Windows or Cygwin executables.
      the compiler will cross-link using the proper libraries.
---------------------------------------------------------
example:

cd /src/local/specpr/config
source ./cshenv.linux64.gfortran
cd /src/local/specpr/src.fstospecpr    # (this directory)
make clean
make
make install

---------------------------------------------------------
