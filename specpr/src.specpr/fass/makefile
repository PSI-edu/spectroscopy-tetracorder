# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ=	\
chkdev.o \
dfasin.o \
rmpath.o

$(SP_OBJ)/fass.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

chkdev.o:		chkdev.r
	$(RF) $(SP_RFLAGS) chkdev.r > chkdev.f
	$(F77) $(SP_FFLAGS) -c chkdev.f
	rm -f chkdev.f

dfasin.o:		dfasin.r
	$(RF) $(SP_RFLAGS) dfasin.r > dfasin.f
	$(F77)  $(SP_FFLAGS) -c dfasin.f
	rm -f dfasin.f

rmpath.o:		rmpath.r
	$(RF) $(SP_RFLAGS) rmpath.r > rmpath.f
	$(F77)  $(SP_FFLAGS) -c rmpath.f
	rm -f rmpath.f
