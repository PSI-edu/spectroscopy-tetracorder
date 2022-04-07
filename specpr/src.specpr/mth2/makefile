# @(#) makefile 2.5@(#)

SHELL=/bin/sh

OBJ=	\
ercomp.o 	\
mathin.o 	\
namdev.o 	\
nwav.o 	\
titles.o 	\
whistr.o 

$(SP_OBJ)/mth2.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@  $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
ercomp.o:		ercomp.r
	$(RF) $(SP_RFLAGS) ercomp.r > ercomp.f
	$(F77) $(SP_FFLAGS) -c ercomp.f
	rm -f ercomp.f

mathin.o:		mathin.r
	$(RF) $(SP_RFLAGS) mathin.r > mathin.f
	$(F77) $(SP_FFLAGS) -c mathin.f
	rm -f mathin.f

namdev.o:		namdev.r
	$(RF) $(SP_RFLAGS) namdev.r > namdev.f
	$(F77) $(SP_FFLAGS) -c namdev.f
	rm -f namdev.f

nwav.o:		nwav.r
	$(RF) $(SP_RFLAGS) nwav.r > nwav.f
	$(F77) $(SP_FFLAGS) -c nwav.f
	rm -f nwav.f

titles.o:		titles.r
	$(RF) $(SP_RFLAGS) titles.r > titles.f
	$(F77) $(SP_FFLAGS) -c titles.f
	rm -f titles.f

whistr.o:		whistr.r
	$(RF) $(SP_RFLAGS) whistr.r > whistr.f
	$(F77) $(SP_FFLAGS) -c whistr.f
	rm -f whistr.f

