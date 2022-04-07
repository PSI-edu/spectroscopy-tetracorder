# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ=	\
redspr.o \
readtx.o \
wrtspr.o

sputil.a:	$(OBJ)
	check_env SP_LIB AR SP_ARFLAGS RANLIB
	rm -f $(SP_LIB)/sputil.a
	$(AR) $(SP_ARFLAGS) $(SP_LIB)/sputil.a $(OBJ)
	$(RANLIB) $(SP_LIB)/sputil.a

.SUFFIXES:
.SUFFIXES: .o .r .c
	
redspr.o:		redspr.r
	check_env SSPP SSPPFLAGS RF SP_RFLAGS F77 SP_FFLAGS
	$(SSPP) -r -$(SSPPFLAGS)  redspr.r > redspr.x
	$(RF) $(SP_RFLAGS) redspr.x > redspr.f
	$(F77) $(SP_FFLAGS) -c redspr.f
	rm -r redspr.x redspr.f

readtx.o:		readtx.r
	check_env SSPP SSPPFLAGS RF SP_RFLAGS F77 SP_FFLAGS
	$(SSPP) -r -$(SSPPFLAGS)  readtx.r > readtx.x
	$(RF) $(SP_RFLAGS) readtx.x > readtx.f
	$(F77) $(SP_FFLAGS) -c readtx.f
	rm -f readtx.x readtx.f 

wrtspr.o:		wrtspr.r
	check_env SSPP SSPPFLAGS RF SP_RFLAGS F77 SP_FFLAGS
	$(SSPP) -r -$(SSPPFLAGS)  wrtspr.r > wrtspr.x
	$(RF) $(SP_RFLAGS) wrtspr.x > wrtspr.f
	$(F77) $(SP_FFLAGS) -c wrtspr.f
	rm -f wrtspr.x wrtspr.f

clean:
	rm -f *.o
