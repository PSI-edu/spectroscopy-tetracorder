# @(#) makefile 2.5@(#)

SHELL=/bin/sh

OBJ= \
f40.o \
f41.o \
xbin.o	\
f42.o \
bandmp.o \
bandmpcv.o \
bdmset.o \
wtochbin.o


$(SP_OBJ)/fcn40-42.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

f40.o:		f40.r
	$(RF) $(SP_RFLAGS) f40.r > f40.f
	$(F77) $(SP_FFLAGS) -c f40.f
	rm -f f40.f

f41.o:		f41.r
	$(RF) $(SP_RFLAGS) f41.r > f41.f
	$(F77) $(SP_FFLAGS) -c f41.f
	rm -f f41.f

xbin.o:		xbin.r
	$(RF) $(SP_RFLAGS) xbin.r > xbin.f
	$(F77) $(SP_FFLAGS) -c xbin.f
	rm -f xbin.f

f42.o:		f42.r
	$(RF) $(SP_RFLAGS) f42.r > f42.f
	$(F77) $(SP_FFLAGS) -c f42.f
	rm -f f42.f

bandmp.o:		bandmp.r
	$(RF) $(SP_RFLAGS) bandmp.r > bandmp.f
	$(F77) $(SP_FFLAGS2) $(SP_FOPT2) -c bandmp.f
	rm -f bandmp.f

bandmpcv.o:		bandmpcv.r
	$(RF) $(SP_RFLAGS) bandmpcv.r > bandmpcv.f
	$(F77) $(SP_FFLAGS2) $(SP_FOPT2) -c bandmpcv.f
	rm -f bandmpcv.f

bdmset.o:		bdmset.r
	$(RF) $(SP_RFLAGS) bdmset.r > bdmset.f
	$(F77) $(SP_FFLAGS) -c bdmset.f
	rm -f bmdset.f

wtochbin.o:		wtochbin.r
	$(RF) $(SP_RFLAGS) wtochbin.r > wtochbin.f
	$(F77) $(SP_FFLAGS) -c wtochbin.f
	rm -f bmdset.f

