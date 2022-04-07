# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ=\
bandel.o \
banins.o \
banmov.o \
banorm.o \
banscr.o 

.SUFFIXES:
.SUFFIXES: .o .r

$(SP_OBJ)/band.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

	
bandel.o:	bandel.r
	$(RF) $(SP_RFLAGS) bandel.r > bandel.f 
	$(F77) $(SP_FFLAGS) -c bandel.f
	rm -f bandel.f
	
banins.o:	banins.r
	$(RF) $(SP_RFLAGS) banins.r > banins.f
	$(F77) $(SP_FFLAGS) -c banins.f
	rm -f banins.f
	
banmov.o:	banmov.r
	$(RF) $(SP_RFLAGS) banmov.r > banmov.f 
	$(F77)  $(SP_FFLAGS) -c banmov.f 
	rm -f banmov.f
	
banorm.o:	banorm.r
	$(RF) $(SP_RFLAGS) banorm.r > banorm.f 
	$(F77) $(SP_FFLAGS) -c banorm.f
	rm -f banorm.f
	
banscr.o:	banscr.r
	$(RF) $(SP_RFLAGS) banscr.r > banscr.f   
	$(F77)  $(SP_FFLAGS) -c banscr.f 
	rm -f banscr.f
