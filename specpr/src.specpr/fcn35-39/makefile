# @(#) makefile 2.2@(#)

SHELL=/bin/sh

OBJ=	\
noise.o \
f38.o   \
f37.o   \
nvres.o   \
bandzmp.o \
bdmzset.o \
f36.o \
f39.o

$(SP_OBJ)/fcn35-39.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

noise.o:	noise.r
	$(RF) $(SP_RFLAGS) noise.r > noise.f 
	$(F77) $(SP_FFLAGS) -c noise.f
	rm -f noise.f

f39.o:		f39.r
	$(RF) $(SP_RFLAGS) f39.r > f39.f 
	$(F77) $(SP_FFLAGS) -c f39.f
	rm -f f39.f

f38.o:		f38.r
	$(RF) $(SP_RFLAGS) f38.r > f38.f 
	$(F77) $(SP_FFLAGS) -c f38.f
	rm -f f38.f

f37.o:		f37.r
	$(RF) $(SP_RFLAGS) f37.r > f37.f 
	$(F77) $(SP_FFLAGS) -c f37.f
	rm -f f37.f

nvres.o:	nvres.r
	$(RF) $(SP_RFLAGS) nvres.r > nvres.f 
	$(F77) $(SP_FFLAGS) -c nvres.f
	rm -f nvres.f

f36.o:		f36.r
	$(RF) $(SP_RFLAGS) f36.r > f36.f 
	$(F77) $(SP_FFLAGS) -c f36.f
	rm -f f36.f

bdmzset.o:	bdmzset.r
	$(RF) $(SP_RFLAGS) bdmzset.r > bdmzset.f 
	$(F77) $(SP_FFLAGS) -c bdmzset.f
	rm -f bdmzset.f

bandzmp.o:	bandzmp.r
	$(RF) $(SP_RFLAGS) bandzmp.r > bandzmp.f 
	$(F77) $(SP_FFLAGS) -c bandzmp.f
	rm -f bandzmp.f

