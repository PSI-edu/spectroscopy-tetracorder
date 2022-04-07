# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
rdspec.o \
rdtitl.o \
readhd.o \
readsp.o \
readwv.o \
recall.o \
writsp.o \
wrtfit.o \
wrtred.o

../io.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../io.o $(OBJ)

rdspec.o:		rdspec.r
	$(RF) $(RFLAGS) rdspec.r > rdspec.f
	$(F77) $(GF2_FFLAGS) -c rdspec.f
	rm -f rdspec.f

rdtitl.o:		rdtitl.r
	$(RF) $(RFLAGS) rdtitl.r > rdtitl.f
	$(F77) $(GF2_FFLAGS) -c rdtitl.f
	rm -f rdtitl.f

readhd.o:		readhd.r
	$(RF) $(RFLAGS) readhd.r > readhd.f
	$(F77) $(GF2_FFLAGS) -c readhd.f
	rm -f readhd.f

readsp.o:		readsp.r
	$(RF) $(RFLAGS) readsp.r > readsp.f
	$(F77) $(GF2_FFLAGS) -c readsp.f
	rm -f readsp.f

readwv.o:		readwv.r
	$(RF) $(RFLAGS) readwv.r > readwv.f
	$(F77) $(GF2_FFLAGS) -c readwv.f
	rm -f readwv.f

recall.o:		recall.r
	$(RF) $(RFLAGS) recall.r > recall.f
	$(F77) $(GF2_FFLAGS) -c recall.f
	rm -f recall.f

writsp.o:		writsp.r
	$(RF) $(RFLAGS) writsp.r > writsp.f
	$(F77) $(GF2_FFLAGS) -c writsp.f
	rm -f writsp.f

wrtfit.o:		wrtfit.r
	$(RF) $(RFLAGS) wrtfit.r > wrtfit.f
	$(F77) $(GF2_FFLAGS) -c wrtfit.f
	rm -f wrtfit.f

wrtred.o:		wrtred.r
	$(RF) $(RFLAGS) wrtred.r > wrtred.f
	$(F77) $(GF2_FFLAGS) -c wrtred.f
	rm -f wrtred.f


