# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
axis.o 	\
catn.o 	\
centim.o	\
curves.o 	\
done.o	\
fnb.o	\
ifnb.o 	\
grid.o 	\
line.o 	\
lnb.o 	\
iopenp.o 	\
pltsym.o 	\
point.o 	\
range.o 	\
round.o 	\
setup.o 	\
snorm.o 	\
swap.o 		\
symbol.o 	\
tname.o 	\
tocent.o 

$(SP_OBJ)/gould.o: $(OBJ)	   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)
	
axis.o:		axis.r
	$(RF) $(SP_RFLAGS) axis.r > axis.f
	$(F77) $(SP_FFLAGS) -c axis.f
	rm -f axis.f

catn.o:		catn.r
	$(RF) $(SP_RFLAGS) catn.r > catn.f
	$(F77) $(SP_FFLAGS) -c catn.f
	rm -f catn.f

centim.o:		centim.r
	$(RF) $(SP_RFLAGS) centim.r > centim.f
	$(F77) $(SP_FFLAGS) -c centim.f
	rm -f centim.f

curves.o:		curves.r
	$(RF) $(SP_RFLAGS) curves.r > curves.f
	$(F77) $(SP_FFLAGS) -c curves.f
	rm -f curves.f

done.o:		done.r
	$(RF) $(SP_RFLAGS) done.r > done.f
	$(F77) $(SP_FFLAGS) -c done.f
	rm -f done.f

fnb.o:		fnb.r
	$(RF) $(SP_RFLAGS) fnb.r > fnb.f
	$(F77) $(SP_FFLAGS) -c fnb.f
	rm -f fnb.f

ifnb.o:		ifnb.r
	$(RF) $(SP_RFLAGS) ifnb.r > ifnb.f
	$(F77) $(SP_FFLAGS) -c ifnb.f
	rm -f ifnb.f

grid.o:		grid.r
	$(RF) $(SP_RFLAGS) grid.r > grid.f
	$(F77) $(SP_FFLAGS) -c grid.f
	rm -f grid.f

line.o:		line.r
	$(RF) $(SP_RFLAGS) line.r > line.f
	$(F77) $(SP_FFLAGS) -c line.f
	rm -f line.f

lnb.o:		lnb.r
	$(RF) $(SP_RFLAGS) lnb.r > lnb.f
	$(F77) $(SP_FFLAGS) -c lnb.f
	rm -f lnb.f

iopenp.o:		iopenp.r
	$(RF) $(SP_RFLAGS) iopenp.r > iopenp.f
	$(F77) $(SP_FFLAGS) -c iopenp.f
	rm -f iopenp.f

pltsym.o:		pltsym.r
	$(RF) $(SP_RFLAGS) pltsym.r > pltsym.f
	$(F77) $(SP_FFLAGS) -c pltsym.f
	rm -f pltsym.f

point.o:		point.r
	$(RF) $(SP_RFLAGS) point.r > point.f
	$(F77) $(SP_FFLAGS) -c point.f
	rm -f point.f

range.o:		range.r
	$(RF) $(SP_RFLAGS) range.r > range.f
	$(F77) $(SP_FFLAGS) -c range.f
	rm -f range.f

round.o:		round.r
	$(RF) $(SP_RFLAGS) round.r > round.f
	$(F77) $(SP_FFLAGS) -c round.f
	rm -f round.f

setup.o:		setup.r
	$(RF) $(SP_RFLAGS) setup.r > setup.f
	$(F77) $(SP_FFLAGS) -c setup.f
	rm -f setup.f

snorm.o:		snorm.r
	$(RF) $(SP_RFLAGS) snorm.r > snorm.f
	$(F77) $(SP_FFLAGS) -c snorm.f
	rm -f snorm.f

swap.o:		swap.r
	$(RF) $(SP_RFLAGS) swap.r > swap.f
	$(F77) $(SP_FFLAGS) -c swap.f
	rm -f swap.f

symbol.o:		symbol.r
	$(RF) $(SP_RFLAGS) symbol.r > symbol.f
	$(F77) $(SP_FFLAGS) -c symbol.f
	rm -f symbol.f

tname.o:		tname.r
	$(RF) $(SP_RFLAGS) tname.r > tname.f
	$(F77) $(SP_FFLAGS) -c tname.f
	rm -f tname.f

tocent.o:		tocent.r
	$(RF) $(SP_RFLAGS) tocent.r > tocent.f
	$(F77) $(SP_FFLAGS) -c tocent.f
	rm -f tocent.f

