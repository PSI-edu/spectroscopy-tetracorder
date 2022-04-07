# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ= \
dlterr.o \
dstdev.o \
runadd.o \
stdev.o \
wegadd.o 

$(SP_OBJ)/add.o:	$(OBJ) ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $(SP_OBJ)/add.o $(OBJ)
	
dlterr.o:	dlterr.r
	$(RF) $(SP_RFLAGS) dlterr.r > dlterr.f
	$(F77) $(SP_FFLAGS) -c dlterr.f
	rm -f dlterr.f

dstdev.o:	dstdev.r
	$(RF) $(SP_RFLAGS) dstdev.r > dstdev.f
	$(F77) $(SP_FFLAGS) -c dstdev.f
	rm -f dstdev.f

stdev.o:	stdev.r
	$(RF) $(SP_RFLAGS) stdev.r > stdev.f
	$(F77) $(SP_FFLAGS) -c stdev.f 
	rm -f stdev.f

runadd.o:	runadd.r
	$(RF) $(SP_RFLAGS) runadd.r > runadd.f
	$(F77) $(SP_FFLAGS) -c runadd.f 
	rm -f runadd.f

wegadd.o:	wegadd.r
	$(RF) $(SP_RFLAGS) wegadd.r > wegadd.f
	$(F77) $(SP_FFLAGS) -c wegadd.f
	rm -f wegadd.f
