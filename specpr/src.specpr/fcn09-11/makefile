# @(#) makefile 2.4@(#)

SHELL=/bin/sh


OBJ=	\
bchop.o 	\
bndrem.o 	\
conrem.o 	\
f9.o 		\
f10.o		\
f11.o 		\
f11dfl.o 	\
f11nrm.o 	\
f11tml.o 	\
filinp.o 


$(SP_OBJ)/fcn09-11.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

bchop.o:		bchop.r
	$(RF) $(SP_RFLAGS) bchop.r > bchop.f
	$(F77) $(SP_FFLAGS) -c bchop.f
	rm -f bchop.f

bndrem.o:		bndrem.r
	$(RF) $(SP_RFLAGS) bndrem.r > bndrem.f
	$(F77) $(SP_FFLAGS) -c bndrem.f
	rm -f bndrem.f

conrem.o:		conrem.r
	$(RF) $(SP_RFLAGS) conrem.r > conrem.f
	$(F77) $(SP_FFLAGS) -c conrem.f
	rm -f conrem.f

f10.o:		f10.r
	$(RF) $(SP_RFLAGS) f10.r > f10.f
	$(F77) $(SP_FFLAGS) -c f10.f
	rm -f f10.f

f9.o:		f9.r
	$(RF) $(SP_RFLAGS) f9.r > f9.f 
	$(F77) $(SP_FFLAGS) -c f9.f
	rm -f f9.f

f11.o:		f11.r
	$(RF) $(SP_RFLAGS) f11.r > f11.f 
	$(F77) $(SP_FFLAGS) -c f11.f
	rm -f f11.f

f11dfl.o:		f11dfl.r
	$(RF) $(SP_RFLAGS) f11dfl.r > f11dfl.f
	$(F77) $(SP_FFLAGS) -c f11dfl.f
	rm -f f11dfl.f

f11nrm.o:		f11nrm.r
	$(RF) $(SP_RFLAGS) f11nrm.r > f11nrm.f
	$(F77) $(SP_FFLAGS) -c f11nrm.f
	rm -f f11nrm.f

f11tml.o:		f11tml.r
	$(RF) $(SP_RFLAGS) f11tml.r > f11tml.f 
	$(F77) $(SP_FFLAGS) -c f11tml.f
	rm -f f11tml.f

filinp.o:		filinp.r
	$(RF) $(SP_RFLAGS) filinp.r > filinp.f
	$(F77) $(SP_FFLAGS) -c filinp.f
	rm -f filinp.f
