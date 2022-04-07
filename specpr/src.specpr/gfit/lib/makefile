# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
delpt.o \
glist.o \
gpar.o \
pgaus.o \
prtfit.o \
ptdel.o \
save.o \
send.o \
shuffl.o \
step.o \
gswap.o \
upprot.o

../lib.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../lib.o $(OBJ)

delpt.o:		delpt.r
	$(RF) $(RFLAGS) delpt.r > delpt.f
	$(F77) $(GF2_FFLAGS) -c delpt.f
	rm -f delpt.f

glist.o:		glist.r
	$(RF) $(RFLAGS) glist.r > glist.f
	$(F77) $(GF2_FFLAGS) -c glist.f
	rm -f glist.f

gpar.o:		gpar.r
	$(RF) $(RFLAGS) gpar.r > gpar.f
	$(F77) $(GF2_FFLAGS) -c gpar.f
	rm -f gpar.f

pgaus.o:		pgaus.r
	$(RF) $(RFLAGS) pgaus.r > pgaus.f
	$(F77) $(GF2_FFLAGS) -c pgaus.f
	rm -f pgaus.f

prtfit.o:		prtfit.r
	$(RF) $(RFLAGS) prtfit.r > prtfit.f
	$(F77) $(GF2_FFLAGS) -c prtfit.f
	rm -f prtfit.f

ptdel.o:		ptdel.r
	$(RF) $(RFLAGS) ptdel.r > ptdel.f
	$(F77) $(GF2_FFLAGS) -c ptdel.f
	rm -f ptdel.f

save.o:		save.r
	$(RF) $(RFLAGS) save.r > save.f
	$(F77) $(GF2_FFLAGS) -c save.f
	rm -f save.f

send.o:		send.r
	$(RF) $(RFLAGS) send.r > send.f
	$(F77) $(GF2_FFLAGS) -c send.f
	rm -f send.f

shuffl.o:		shuffl.r
	$(RF) $(RFLAGS) shuffl.r > shuffl.f
	$(F77) $(GF2_FFLAGS) -c shuffl.f
	rm -f shuffl.f

step.o:		step.r
	$(RF) $(RFLAGS) step.r > step.f
	$(F77) $(GF2_FFLAGS) -c step.f
	rm -f step.f

gswap.o:		gswap.r
	$(RF) $(RFLAGS) gswap.r > gswap.f
	$(F77) $(GF2_FFLAGS) -c gswap.f
	rm -f gswap.f

upprot.o:		upprot.r
	$(RF) $(RFLAGS) upprot.r > upprot.f
	$(F77) $(GF2_FFLAGS) -c upprot.f
	rm -f upprot.f

