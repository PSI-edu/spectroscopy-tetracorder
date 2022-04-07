# @(#) makefile 2.6@(#)

SHELL=/bin/sh

OBJ= \
convol.o \
delx.o \
gfiles.o \
ggauss.o \
f17.o \
f18.o \
determ.o  \
f19.o \
polfit.o 


$(SP_OBJ)/fcn17-19.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)


convol.o:		convol.r
	$(RF) $(SP_RFLAGS) convol.r > convol.f
	$(F77) $(SP_FFLAGS) -c convol.f
	rm -f convol.f

delx.o:		delx.r
	$(RF) $(SP_RFLAGS) delx.r > delx.f
	$(F77) $(SP_FFLAGS) -c delx.f
	rm -f delx.f

gfiles.o:		gfiles.r
	$(RF) $(SP_RFLAGS) gfiles.r > gfiles.f
	$(F77) $(SP_FFLAGS) -c gfiles.f
	rm -f gfiles.f

ggauss.o:		ggauss.r
	$(RF) $(SP_RFLAGS) ggauss.r > ggauss.f
	$(F77) $(SP_FFLAGS) -c ggauss.f
	rm -f ggauss.f

f17.o:		f17.r
	$(RF) $(SP_RFLAGS) f17.r > f17.f
	$(F77) $(SP_FFLAGS) -c f17.f
	rm -f f17.f

f18.o:		f18.r
	$(RF) $(SP_RFLAGS) f18.r > f18.f
	$(F77) $(SP_FFLAGS) -c f18.f
	rm -f f18.f

determ.o:		determ.r
	$(RF) $(SP_RFLAGS) determ.r > determ.f
	$(F77) $(SP_FFLAGS) -c determ.f
	rm -f determ.f

f19.o:		f19.r
	$(RF) $(SP_RFLAGS) f19.r > f19.f 
	$(F77) $(SP_FFLAGS) -c f19.f
	rm -f f19.f

polfit.o:	polfit.r
	$(RF) $(SP_RFLAGS) polfit.r > polfit.f 
	$(F77) $(SP_FFLAGS) -c polfit.f
	rm -f polfit.f

