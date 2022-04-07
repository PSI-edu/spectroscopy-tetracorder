# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
contrl.o \
normal.o

../contrl.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../contrl.o $(OBJ)

contrl.o:		contrl.r
	$(RF) $(RFLAGS) contrl.r > contrl.f
	$(F77) $(GF2_FFLAGS) -c contrl.f
	rm -f contrl.f

normal.o:		normal.r
	$(RF) $(RFLAGS) normal.r > normal.f
	$(F77) $(GF2_FFLAGS) -c normal.f
	rm -f normal.f

