SHELL=/bin/sh
SRCFIL=../..

OBJ=		\
spsetwave.o

# no longer needed in HPUX
#../../../hpux_fortran/iargc.o	\
#../../../hpux_fortran/getarg.o
#

spsetwave:	$(OBJ)  $(SP_LIB)/specpr.a $(SP_LIB)/sputil.a 
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o spsetwave $(OBJ) $(SP_LIB)/specpr.a  \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


spsetwave.o:		spsetwave.r
	$(SSPP) -r -$(SSPPFLAGS)  spsetwave.r > spsetwave.x
	$(RF) $(SP_RFLAGS) spsetwave.x > spsetwave.f
	$(F77) $(SP_FFLAGS) -c spsetwave.f
	rm -f spsetwave.x  spsetwave.f

install:	
	mv spsetwave $(SP_LOCAL)/bin/spsetwave
	chmod 755 $(SP_LOCAL)/bin/spsetwave
