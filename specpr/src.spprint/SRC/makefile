SHELL=/bin/sh
SRCFIL=../..

OBJ=		\
cprint2.o	\
spprint.o

# no longer needed in HPUX
#../../../hpux_fortran/iargc.o	\
#../../../hpux_fortran/getarg.o
#

spprint:	$(OBJ)
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o spprint $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


spprint.o:		spprint.r
	$(SSPP) -r -$(SSPPFLAGS)  spprint.r > spprint.x
	$(RF) $(SP_RFLAGS) spprint.x > spprint.f
	$(F77) $(SP_FFLAGS) -c spprint.f
	rm -f spprint.x spprint.f

cprint2.o:		cprint2.r
	$(SSPP) -r -$(SSPPFLAGS)  cprint2.r > cprint2.x
	$(RF) $(SP_RFLAGS) cprint2.x > cprint2.f
	$(F77) $(SP_FFLAGS) -c cprint2.f
	rm -f cprint2.x cprint2.f

install:	
	mv spprint $(SP_LOCAL)/bin/spprint
	chmod 755 $(SP_LOCAL)/bin/spprint
