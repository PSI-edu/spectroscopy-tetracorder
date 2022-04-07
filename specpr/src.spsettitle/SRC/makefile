SHELL=/bin/sh
SRCFIL=../..

OBJ=		\
spsettitle.o

# no longer needed in HPUX
#../../../hpux_fortran/iargc.o	\
#../../../hpux_fortran/getarg.o
#

spsettitle:	$(OBJ)  $(SP_LIB)/specpr.a $(SP_LIB)/sputil.a 
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o spsettitle $(OBJ) $(SP_LIB)/specpr.a  \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


spsettitle.o:		spsettitle.r
	$(SSPP) -r -$(SSPPFLAGS)  spsettitle.r > spsettitle.x
	$(RF) $(SP_RFLAGS) spsettitle.x > spsettitle.f
	$(F77) $(SP_FFLAGS) -c spsettitle.f
	rm -f spsettitle.x  spsettitle.f

install:	
	mv spsettitle $(SP_LOCAL)/bin/spsettitle
	chmod 755 $(SP_LOCAL)/bin/spsettitle
