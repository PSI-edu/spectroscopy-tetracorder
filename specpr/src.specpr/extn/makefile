# @(#) makefile 2.4@(#)

SHELL=/bin/sh
GET=sp_if_get

OBJ=	\
extnct.o 	\
extnta.o 	\
extplt.o 	\
iplot.o 	\
linfit.o 

$(SP_OBJ)/extn.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

extnct.o:		extnct.r
	$(RF) $(SP_RFLAGS) extnct.r > extnct.f
	$(F77) $(SP_FFLAGS) -c extnct.f
	rm -f extnct.f

extnta.o:		extnta.r
	$(RF) $(SP_RFLAGS) extnta.r > extnta.f
	$(F77) $(SP_FFLAGS) -c extnta.f
	rm -f extnta.f

extplt.o:		extplt.r
	$(RF) $(SP_RFLAGS) extplt.r > extplt.f 
	$(F77) $(SP_FFLAGS) -c extplt.f
	rm -f extplt.f

iplot.o:		iplot.r
	$(RF) $(SP_RFLAGS) iplot.r > iplot.f
	$(F77) $(SP_FFLAGS) -c iplot.f
	rm -f iplot.f

linfit.o:		linfit.r
	$(RF) $(SP_RFLAGS) linfit.r > linfit.f
	$(F77) $(SP_FFLAGS) -c linfit.f
	rm -f linfit.f
