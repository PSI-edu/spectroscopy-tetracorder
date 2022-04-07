# @(#) makefile 2.1@(#)

SHELL=/bin/sh

OBJ= \
f48.o \
f49.o	\
f49wav.o \
pltdat.o \
pltxtx.o \
tabpos.o \
getcor.o \
getpt.o \
f50.o \
gtfl.o \
lngen.o \
plot2.o

#gcrd.o


$(SP_OBJ)/fcn48-50.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)


f48.o:		f48.r
	$(RF) $(SP_RFLAGS) f48.r > f48.f
	$(F77) $(SP_FFLAGS) -c f48.f
	rm -f f48.f

tabpos.o:		tabpos.r
	$(RF) $(SP_RFLAGS) tabpos.r > tabpos.f
	$(F77) $(SP_FFLAGS) -c tabpos.f
	rm -f tabpos.f

getcor.o:		getcor.r
	$(RF) $(SP_RFLAGS) getcor.r > getcor.f
	$(F77) $(SP_FFLAGS) -c getcor.f
	rm -f getcor.f

getpt.o:		getpt.r
	$(RF) $(SP_RFLAGS) getpt.r > getpt.f
	$(F77) $(SP_FFLAGS) -c getpt.f
	rm -f getpt.f

pltdat.o:		pltdat.r
	$(RF) $(SP_RFLAGS) pltdat.r > pltdat.f
	$(F77) $(SP_FFLAGS) -c pltdat.f
	rm -f pltdat.f

pltxtx.o:		pltxtx.r
	$(RF) $(SP_RFLAGS) pltxtx.r > pltxtx.f
	$(F77) $(SP_FFLAGS) -c pltxtx.f
	rm -f pltxtx.f

f49.o:		f49.r
	$(RF) $(SP_RFLAGS) f49.r > f49.f
	$(F77) $(SP_FFLAGS) -c f49.f
	rm -f f49.f

f49wav.o:		f49wav.r
	$(RF) $(SP_RFLAGS) f49wav.r > f49wav.f
	$(F77) $(SP_FFLAGS) -c f49wav.f
	rm -f f49wav.f

f50.o:		f50.r
	$(RF) $(SP_RFLAGS) f50.r > f50.f
	$(F77) $(SP_FFLAGS) -c f50.f
	rm -f f50.f

gcrd.o:		gcrd.r
	$(RF) $(SP_RFLAGS) gcrd.r > gcrd.f
	$(F77) $(SP_FFLAGS) -c gcrd.f
	rm -f gcrd.f

gtfl.o:		gtfl.r
	$(RF) $(SP_RFLAGS) gtfl.r > gtfl.f
	$(F77) $(SP_FFLAGS) -c gtfl.f
	rm -f gtfl.f

lngen.o:		lngen.r
	$(RF) $(SP_RFLAGS) lngen.r > lngen.f
	$(F77) $(SP_FFLAGS) -c lngen.f
	rm -f lngen.f

plot2.o:		plot2.r
	$(RF) $(SP_RFLAGS) plot2.r > plot2.f
	$(F77) $(SP_FFLAGS) -c plot2.f
	rm -f plot2.f

