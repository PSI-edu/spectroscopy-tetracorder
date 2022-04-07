# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ= \
mulrun.o \
rundiv.o 

$(SP_OBJ)/dvml.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

mulrun.o:		mulrun.r
	$(RF) $(SP_RFLAGS) mulrun.r > mulrun.f 
	$(F77) $(SP_FFLAGS) -c mulrun.f
	rm -f mulrun.f

rundiv.o:		rundiv.r
	$(RF) $(SP_RFLAGS) rundiv.r > rundiv.f
	$(F77) $(SP_FFLAGS) -c rundiv.f
	rm -f rundiv.f
