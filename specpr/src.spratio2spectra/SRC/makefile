SHELL=/bin/sh
SRCFIL=../..

OBJ=		\
spratio2spectra.o

# no longer needed in HPUX
#../../../hpux_fortran/iargc.o	\
#../../../hpux_fortran/getarg.o
#

spratio2spectra:	$(OBJ)  $(SP_LIB)/specpr.a $(SP_LIB)/sputil.a 
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o spratio2spectra $(OBJ) $(SP_LIB)/specpr.a  \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


spratio2spectra.o:		spratio2spectra.r
	$(SSPP) -r -$(SSPPFLAGS)  spratio2spectra.r > spratio2spectra.x
	$(RF) $(SP_RFLAGS) spratio2spectra.x > spratio2spectra.f
	$(F77) $(SP_FFLAGS) -c spratio2spectra.f
	rm -f spratio2spectra.x  spratio2spectra.f

install:	spratio2spectra spratio2spectra.o spratio2spectra.r 
	cp -a spratio2spectra $(SP_LOCAL)/bin/spratio2spectra
	chmod 755 $(SP_LOCAL)/bin/spratio2spectra
