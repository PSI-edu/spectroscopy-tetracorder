MAKE=make
MAKE2=make fit.o

.SUFFIXES:
.SUFFIXES: .o	.r	.c

.r.o:
	@echo "***WARNING***" $*.o is out of date

.c.o:

LIBS=\
change.o\
io.o\
lib.o\
fits.o\
rstart.o

DIRS=\
change contrl fit io lib main plot rstart

all: gfit fits

gfit: main/gfit.o $(SP_LIBS) plot.o
	$(F77) $(GF1_LFLAGS) $(SP_LDFLAGS) -o gfit $(LIBS) main/gfit.o $(SP_LDLIBS) plot.o $(SP_LIB)/specpr.a $(LIBPW)


fits: fit/fit.o $(LIBS) contrl.o
	$(F77) $(GF1_LFLAGS) $(SP_LDFLAGS) -o fits $(LIBS) fit/fit.o $(SP_LDLIBS) contrl.o $(SP_LIB)/specpr.a $(LIBPW)


main/gfit.o: main/gfit.r
		(cd main; $(MAKE) )

fit/fit.o: fit/fit.r
		(cd fit; $(MAKE2) )

change.o:\
change/chgest.r     change/chgfit.r     change/chgues.r     
		(cd change; $(MAKE) )

contrl.o:\
contrl/contrl.r contrl/normal.r     
		(cd contrl; $(MAKE) )

fits.o:\
fit/bas1.r          fit/f.r				fit/matinv.r
		(cd fit; $(MAKE) )

io.o:\
io/rdspec.r         io/rdtitl.r 		io/readhd.r\
io/readsp.r         io/readwv.r         io/recall.r \
io/writsp.r			io/wrtfit.r			io/wrtred.r
		(cd io; $(MAKE) )

lib.o:\
lib/delpt.r 		lib/gpar.r          lib/glist.r	\
lib/pgaus.r         lib/prtfit.r 		lib/ptdel.r  \
lib/save.r          lib/send.r          lib/shuffl.r \
lib/step.r          lib/upprot.r 	    lib/gswap.r
		(cd lib; $(MAKE) )

plot.o:\
plot/gfplot.r       plot/plot.r 		plot/ptplot.r       \
plot/scale.r        plot/xplta.r        
		(cd plot; $(MAKE) )

rstart.o:\
rstart/gstart.r 	rstart/transfer.r 
		(cd rstart; $(MAKE) )

install:
	cp gfit /usr/local/bin/gfit
	cp fits /usr/local/bin/fits

clean:
	rm -f *.o; \
	for i in $(DIRS) ;\
	do \
		(cd $$i;echo "Cleaning $$i";rm -f *.o);\
	done
