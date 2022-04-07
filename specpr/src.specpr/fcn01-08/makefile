# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
comprs.o 	\
dtsort.o 	\
f1.o 	\
f2.o 	\
f3.o 	\
f4.o 	\
f5.o 	\
f6.o 	\
f7.o 	\
f8.o 	\
manhst.o 	\
planck.o 

$(SP_OBJ)/fcn01-08.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

comprs.o:		comprs.r
	$(RF) $(SP_RFLAGS) comprs.r > comprs.f 
	$(F77) $(SP_FFLAGS) -c comprs.f
	rm -f comprs.f

dtsort.o:		dtsort.r
	$(RF) $(SP_RFLAGS) dtsort.r > dtsort.f 
	$(F77) $(SP_FFLAGS) -c dtsort.f
	rm -f dtsort.f

f1.o:		f1.r
	$(RF) $(SP_RFLAGS) f1.r > f1.f
	$(F77) $(SP_FFLAGS) -c f1.f
	rm -f f1.f

f2.o:		f2.r
	$(RF) $(SP_RFLAGS) f2.r > f2.f
	$(F77) $(SP_FFLAGS) -c f2.f
	rm -f f2.f

f3.o:		f3.r
	$(RF) $(SP_RFLAGS) f3.r > f3.f
	$(F77) $(SP_FFLAGS) -c f3.f
	rm -f f3.f

f4.o:		f4.r
	$(RF) $(SP_RFLAGS) f4.r > f4.f
	$(F77) $(SP_FFLAGS) -c f4.f
	rm -f f4.f

f5.o:		f5.r
	$(RF) $(SP_RFLAGS) f5.r > f5.f
	$(F77) $(SP_FFLAGS) -c f5.f
	rm -f f5.f

f6.o:		f6.r
	$(RF) $(SP_RFLAGS) f6.r > f6.f
	$(F77) $(SP_FFLAGS) -c f6.f
	rm -f f6.f

f7.o:		f7.r
	$(RF) $(SP_RFLAGS) f7.r > f7.f
	$(F77) $(SP_FFLAGS) -c f7.f
	rm -f f7.f

f8.o:		f8.r
	$(RF) $(SP_RFLAGS) f8.r > f8.f
	$(F77) $(SP_FFLAGS) -c f8.f
	rm -f f8.f

manhst.o:		manhst.r
	$(RF) $(SP_RFLAGS) manhst.r > manhst.f
	$(F77) $(SP_FFLAGS) -c manhst.f
	rm -f manhst.f

planck.o:		planck.r
	$(RF) $(SP_RFLAGS) planck.r > planck.f
	$(F77) $(SP_FFLAGS) -c planck.f
	rm -f planck.f
