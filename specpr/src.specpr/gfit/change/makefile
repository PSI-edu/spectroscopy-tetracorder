# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
chgest.o \
chgfit.o \
chgues.o

../change.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../change.o $(OBJ)

chgest.o:		chgest.r
	$(RF) $(RFLAGS) chgest.r > chgest.f
	$(F77) $(GF2_FFLAGS) -c chgest.f
	rm -f chgest.f

chgfit.o:		chgfit.r
	$(RF) $(RFLAGS) chgfit.r > chgfit.f
	$(F77) $(GF2_FFLAGS) -c chgfit.f
	rm -f chgfit.f

chgues.o:		chgues.r
	$(RF) $(RFLAGS) chgues.r > chgues.f
	$(F77) $(GF2_FFLAGS) -c chgues.f
	rm -f chgues.f

