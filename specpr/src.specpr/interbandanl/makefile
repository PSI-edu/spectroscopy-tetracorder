# @(#) makefile 2.9@(#)

SHELL=/bin/sh
GET=sp_if_get

OBJ= \
bdanal.o \
box.o \
quad.o \
findrf.o \
findwd.o \
sfwhm.o \
mindat.o \
restab.o \
replot.o \
rbline.o \
dconvt.o \
gconvt.o \
bcchan.o \
opfile.o        
#getpos.o 


$(SP_OBJ)/interbandanl.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r

bdanal.o:		bdanal.r
	$(RF) $(SP_RFLAGS) bdanal.r > bdanal.f
	$(F77) $(SP_FFLAGS) -c bdanal.f
	rm -f bdanal.f

box.o:		box.r
	$(RF) $(SP_RFLAGS) box.r > box.f
	$(F77) $(SP_FFLAGS)  -c box.f
	rm -f box.f

opfile.o:		opfile.r
	$(RF) $(SP_RFLAGS) opfile.r > opfile.f
	$(F77) $(SP_FFLAGS) -c opfile.f
	rm -f opfile.f

getpos.o:		getpos.r
	$(RF) $(SP_RFLAGS) getpos.r > getpos.f
	$(F77) $(SP_FFLAGS)  -c getpos.f
	rm -f getpos.f

bcchan.o:		bcchan.r
	$(RF) $(SP_RFLAGS) bcchan.r > bcchan.f
	$(F77) $(SP_FFLAGS)  -c bcchan.f
	rm -f bcchan.f

dconvt.o:		dconvt.r
	$(RF) $(SP_RFLAGS) dconvt.r > dconvt.f
	$(F77) $(SP_FFLAGS) -c dconvt.f
	rm -f dconvt.f

gconvt.o:		gconvt.r
	$(RF) $(SP_RFLAGS) gconvt.r > gconvt.f
	$(F77) $(SP_FFLAGS) -c gconvt.f
	rm -f gconvt.f

quad.o:		quad.r
	$(RF) $(SP_RFLAGS) quad.r > quad.f
	$(F77) $(SP_FFLAGS)  -c quad.f
	rm -f quad.f

findrf.o:		findrf.r
	$(RF) $(SP_RFLAGS) findrf.r > findrf.f
	$(F77) $(SP_FFLAGS)  -c findrf.f
	rm -f findrf.f

findwd.o:		findwd.r
	$(RF) $(SP_RFLAGS) findwd.r > findwd.f
	$(F77) $(SP_FFLAGS)  -c findwd.f
	rm -f findwd.f

sfwhm.o:		sfwhm.r
	$(RF) $(SP_RFLAGS) sfwhm.r > sfwhm.f
	$(F77) $(SP_FFLAGS)  -c sfwhm.f
	rm -f sfwhm.f

mindat.o:		mindat.r
	$(RF) $(SP_RFLAGS) mindat.r > mindat.f
	$(F77) $(SP_FFLAGS)  -c mindat.f
	rm -f mindat.f

restab.o:		restab.r
	$(RF) $(SP_RFLAGS) restab.r > restab.f
	$(F77) $(SP_FFLAGS)  -c restab.f
	rm -f restab.f

replot.o:		replot.r
	$(SSPP) -r -$(SSPPFLAGS)  replot.r >replot.x
	$(RF) $(SP_RFLAGS) replot.x > replot.f
	$(F77) $(SP_FFLAGS) -c replot.f
	rm -f replot.x replot.f

rbline.o:		rbline.r
	$(RF) $(SP_RFLAGS) rbline.r > rbline.f
	$(F77) $(SP_FFLAGS) -c rbline.f
	rm -f rbline.f

