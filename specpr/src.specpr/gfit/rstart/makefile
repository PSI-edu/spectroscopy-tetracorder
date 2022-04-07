# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
gstart.o \
transfer.o

../rstart.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../rstart.o $(OBJ)

gstart.o:		gstart.r
	$(RF) $(RFLAGS) gstart.r > gstart.f
	$(F77) $(GF2_FFLAGS) -c gstart.f
	rm -f gstart.f

transfer.o:		transfer.r
	$(RF) $(RFLAGS) transfer.r > transfer.f
	$(F77) $(GF2_FFLAGS) -c transfer.f
	rm -f transfer.f

grstart.o:		grstart.r
	$(RF) $(RFLAGS) grstart.r > grstart.f
	$(F77) $(GF2_FFLAGS) -c grstart.f
	rm -f grstart.f

