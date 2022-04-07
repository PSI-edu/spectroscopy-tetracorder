# @(#) makefile 2.1@(#)

SHELL=/bin/sh

OBJ= \
f51.o

#gcrd.o


$(SP_OBJ)/fcn61-52.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)


f51.o:		f51.r
	$(RF) $(SP_RFLAGS) f51.r > f51.f
	$(F77) $(SP_FFLAGS) -c f51.f
	rm -f f51.f
