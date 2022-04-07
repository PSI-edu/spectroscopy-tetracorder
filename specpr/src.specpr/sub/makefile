# @(#) makefile 2.5@(#)

SHELL=/bin/sh

OBJ=	\
radec.o 	\
runsb1.o 	\
runsub.o 

$(SP_OBJ)/sub.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
radec.o:		radec.r
	$(RF) $(SP_RFLAGS) radec.r > radec.f
	$(F77) $(SP_FFLAGS) -c radec.f
	rm -f radec.f


runsb1.o:		runsb1.r
	$(RF) $(SP_RFLAGS) runsb1.r > runsb1.f
	$(F77) $(SP_FFLAGS) -c runsb1.f
	rm -f runsb1.f


runsub.o:		runsub.r
	$(RF) $(SP_RFLAGS) runsub.r > runsub.f
	$(F77) $(SP_FFLAGS) -c runsub.f
	rm -f runsub.f
