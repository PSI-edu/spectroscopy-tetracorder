# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
f20.o 	\
f21.o 	\
f22.o 	\
f24.o 	\
f24dfl.o 	\
f24nrm.o 	\
f24tml.o 	\
f25.o		\
lsqft.o	

$(SP_OBJ)/fcn20-25.o:	$(OBJ)	   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

f20.o:		f20.r
	$(SSPP) -r -$(SSPPFLAGS) f20.r > f20.x
	$(RF) $(SP_RFLAGS) f20.x > f20.f
	$(F77) $(SP_FFLAGS) -c f20.f
	rm -f f20.x f20.f

f21.o:		f21.r
	$(RF) $(SP_RFLAGS) f21.r > f21.f
	$(F77) $(SP_FFLAGS) -c f21.f
	rm -f f21.f

f22.o:		f22.r
	$(RF) $(SP_RFLAGS) f22.r > f22.f
	$(F77) $(SP_FFLAGS) -c f22.f
	rm -f f22.f

f24.o:		f24.r
	$(RF) $(SP_RFLAGS) f24.r > f24.f
	$(F77) $(SP_FFLAGS) -c f24.f
	rm -f f24.f

f24dfl.o:		f24dfl.r
	$(RF) $(SP_RFLAGS) f24dfl.r > f24dfl.f
	$(F77) $(SP_FFLAGS) -c f24dfl.f
	rm -f f24dfl.f

f24nrm.o:		f24nrm.r
	$(RF) $(SP_RFLAGS) f24nrm.r > f24nrm.f
	$(F77) $(SP_FFLAGS) -c f24nrm.f
	rm -f f24nrm.f

f24tml.o:		f24tml.r
	$(RF) $(SP_RFLAGS) f24tml.r > f24tml.f
	$(F77) $(SP_FFLAGS) -c f24tml.f 
	rm -f f24tml.f

f25.o:		f25.r
	$(RF) $(SP_RFLAGS) f25.r > f25.f
	$(F77) $(SP_FFLAGS) -c f25.f
	rm -f f25.f

lsqft.o:		lsqft.r
	$(RF) $(SP_RFLAGS) lsqft.r > lsqft.f
	$(F77) $(SP_FFLAGS) -c lsqft.f
	rm -f lsqft.f
