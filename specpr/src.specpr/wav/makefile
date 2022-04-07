# @(#) makefile 2.10@(#)

SHELL=/bin/sh

OBJ=	\
autowv.o	\
iwidok.o	\
lcasew.o	\
namdwv.o	\
redwav.o	\
wavlng.o 	\
setwpt.o	\
wriwav.o 

$(SP_OBJ)/wav.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
autowv.o:		autowv.r
	$(RF) $(SP_RFLAGS) autowv.r > autowv.f
	$(F77) $(SP_FFLAGS) -c autowv.f
	rm -f autowv.f

iwidok.o:		iwidok.r
	$(RF) $(SP_RFLAGS) iwidok.r > iwidok.f
	$(F77) $(SP_FFLAGS) -c iwidok.f
	rm -f iwidok.f

lcasew.o:		lcasew.r
	$(RF) $(SP_RFLAGS) lcasew.r > lcasew.f
	$(F77) $(SP_FFLAGS) -c lcasew.f
	rm -f lcasew.f

namdwv.o:		namdwv.r
	$(RF) $(SP_RFLAGS) namdwv.r > namdwv.f
	$(F77) $(SP_FFLAGS) -c namdwv.f
	rm -f namdwv.f

redwav.o:		redwav.r
	$(SSPP) -r -$(SSPPFLAGS) redwav.r > redwav.x
	$(RF) $(SP_RFLAGS) redwav.x > redwav.f
	$(F77) $(SP_FFLAGS) -c redwav.f
	rm -f redwav.x redwav.f

wavlng.o:		wavlng.r
	$(RF) $(SP_RFLAGS) wavlng.r > wavlng.f
	$(F77) $(SP_FFLAGS) -c wavlng.f
	rm -f wavlng.f

wriwav.o:		wriwav.r
	$(RF) $(SP_RFLAGS) wriwav.r > wriwav.f
	$(F77) $(SP_FFLAGS) -c wriwav.f
	rm -f wriwav.f

setwpt.o:		setwpt.r
	$(RF) $(SP_RFLAGS) setwpt.r > setwpt.f
	$(F77) $(SP_FFLAGS) -c setwpt.f
	rm -f setwpt.f
