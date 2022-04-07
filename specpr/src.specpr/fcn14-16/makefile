# @(#) makefile 2.5@(#)

SHELL=/bin/sh

OBJ=	\
chdata.o 	\
chdel.o 	\
chinst.o 	\
chlist.o 	\
chprnt.o 	\
f14.o \
f15.o \
f15cde.o \
f16.o 


$(SP_OBJ)/fcn14-16.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

chdata.o:		chdata.r
	$(RF) $(SP_RFLAGS) chdata.r > chdata.f
	$(F77) $(SP_FFLAGS) -c chdata.f
	rm -f chdata.f

chdel.o:		chdel.r
	$(RF) $(SP_RFLAGS) chdel.r > chdel.f 
	$(F77) $(SP_FFLAGS) -c chdel.f
	rm -f chdel.f

chinst.o:		chinst.r
	$(RF) $(SP_RFLAGS) chinst.r > chinst.f
	$(F77) $(SP_FFLAGS) -c chinst.f
	rm -f chinst.f

chlist.o:		chlist.r
	$(RF) $(SP_RFLAGS) chlist.r > chlist.f
	$(F77) $(SP_FFLAGS) -c chlist.f
	rm -f chlist.f

chprnt.o:		chprnt.r
	$(RF) $(SP_RFLAGS) chprnt.r > chprnt.f 
	$(F77) $(SP_FFLAGS) -c chprnt.f
	rm -f chprnt.f

f14.o:		f14.r
	$(RF) $(SP_RFLAGS) f14.r > f14.f
	$(F77) $(SP_FFLAGS) -c f14.f
	rm -f f14.f

f15.o:		f15.r
	$(RF) $(SP_RFLAGS) f15.r > f15.f 
	$(F77) $(SP_FFLAGS) -c f15.f
	rm -f f15.f

f15cde.o:		f15cde.r
	$(RF) $(SP_RFLAGS) f15cde.r > f15cde.f 
	$(F77) $(SP_FFLAGS) -c f15cde.f
	rm -f f15cde.f

f16.o:		f16.r
	$(RF) $(SP_RFLAGS) f16.r > f16.f
	$(F77) $(SP_FFLAGS) -c f16.f
	rm -f f16.f
