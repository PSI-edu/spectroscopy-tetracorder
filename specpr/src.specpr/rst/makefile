# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=			\
	rstdmp.o	\
	rstart.o	\
	rstart0.o

$(SP_OBJ)/rst.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

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

rstart0.o:		rstart0.r
	$(RF) $(SP_RFLAGS) rstart0.r > rstart0.f
	$(F77) $(SP_FFLAGS) -c rstart0.f
	rm -f rstart0.f
