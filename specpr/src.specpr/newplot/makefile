# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
bldplt.o	\
gbldel.o 	\
getplt.o 	\
gettxt.o 	\
gpplot.o 	\
gtrecs.o	\
haxis.o		\
options.o 	\
savdta.o 	\
sclfct.o 	\
spinit.o	\
setwav.o	\
doplot.o	\
vaxis.o

$(SP_OBJ)/newplot.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
bldplt.o:		bldplt.r
	$(RF) $(SP_RFLAGS) bldplt.r > bldplt.f
	$(F77) $(SP_FFLAGS) -c bldplt.f
	rm -f bldplt.f

gbldel.o:		gbldel.r
	$(RF) $(SP_RFLAGS) gbldel.r > gbldel.f
	$(F77) $(SP_FFLAGS) -c gbldel.f
	rm -f gbldel.f

getplt.o:		getplt.r
	$(RF) $(SP_RFLAGS) getplt.r > getplt.f
	$(F77) $(SP_FFLAGS) -c getplt.f
	rm -f getplt.f

gettxt.o:		gettxt.r
	$(RF) $(SP_RFLAGS) gettxt.r > gettxt.f
	$(F77) $(SP_FFLAGS) -c gettxt.f
	rm -f gettxt.f

gpplot.o:		gpplot.r
	$(RF) $(SP_RFLAGS) gpplot.r > gpplot.f
	$(F77) $(SP_FFLAGS) -c gpplot.f
	rm -f gpplot.f

gtrecs.o:		gtrecs.r
	$(RF) $(SP_RFLAGS) gtrecs.r > gtrecs.f
	$(F77) $(SP_FFLAGS) -c gtrecs.f
	rm -f gtrecs.f

haxis.o:		haxis.r
	$(RF) $(SP_RFLAGS) haxis.r > haxis.f
	$(F77) $(SP_FFLAGS) -c haxis.f
	rm -f haxis.f

options.o:		options.r
	$(RF) $(SP_RFLAGS) options.r > options.f
	$(F77) $(SP_FFLAGS) -c options.f
	rm -f options.f

savdta.o:		savdta.r
	$(RF) $(SP_RFLAGS) savdta.r > savdta.f
	$(F77) $(SP_FFLAGS) -c savdta.f
	rm -f savdta.f

sclfct.o:		sclfct.r
	$(RF) $(SP_RFLAGS) sclfct.r > sclfct.f
	$(F77) $(SP_FFLAGS) -c sclfct.f
	rm -f sclfct.f

spinit.o:		spinit.r
	$(RF) $(SP_RFLAGS) spinit.r > spinit.f
	$(F77) $(SP_FFLAGS) -c spinit.f
	rm -f spinit.f

setwav.o:		setwav.r
	$(RF) $(SP_RFLAGS) setwav.r > setwav.f
	$(F77) $(SP_FFLAGS) -c setwav.f
	rm -f setwav.f

doplot.o:		doplot.r
	$(RF) $(SP_RFLAGS) doplot.r > doplot.f
	$(F77) $(SP_FFLAGS) -c doplot.f
	rm -f doplot.f

vaxis.o:		vaxis.r
	$(RF) $(SP_RFLAGS) vaxis.r > vaxis.f
	$(F77) $(SP_FFLAGS) -c vaxis.f
	rm -f vaxis.f
