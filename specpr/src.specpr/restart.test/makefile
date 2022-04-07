# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
	rstdmp.o	\
	rstart.o

$(SP_OBJ)/rst.o:	$(OBJ)   ../common/spblockdata.o
	ld $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
rstdmp.o:		rstdmp.r
	$(RF) $(SP_RFLAGS) rstdmp.r > rstdmp.f
	$(F77) $(SP_FFLAGS) -c rstdmp.f
	rm -f rstdmp.f


rstart.o:		rstart.r
	$(RF) $(SP_RFLAGS) rstart.r > rstart.f
	$(F77) $(SP_FFLAGS) -c rstart.f
	rm -f rstart.f
