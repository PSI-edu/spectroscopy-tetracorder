SHELL=/bin/sh

OBJ=		\
radtran.o	\
abscf.o		\
getns.o		\
getspec.o	\
getdfp.o	\
getembn.o	\
getembk.o	\
kweigh.o	\
abpeak.o	\
getmu.o		\
calcr.o		\
cnvg1g.o	\
cnvgbr.o	\
nweigh.o	\
kfind.o		\
spcwri.o	\
spctwri.o	\
wavavg.o	\
nfind.o		\
kiter.o		\
plotit.o	\
getwav.o	\
getnset.o	\
dwcomp.o	\
getk.o		\
getn.o		\
getdefs.o	\
mrefl3.o	\
csatrd.o	\
cxband.o	\
xccont.o	\
eval1g.o	\
bdrefl.o	\
stdref.o	\
ssube.o		\
ssube1981.o		\
ssubi.o		\
ssubi1981.o		\
qsmnh.o		\
qsubs.o		\
hr.o		\
refl.o		\
singscat.o      \
unmix.o		\
refmix.o	\
embedmix.o	\
getk2.o		\
getn2.o		\
getnst2.o	\
refcom.o	\
f_Rpara.o	\
f_Rperp.o	\
f_Se_pg59.o	\
f_Se_pg60.o	\
calcrlyr.o	\
mrefl3lyr.o	\
reflyrmix.o	\
getlayerthick.o	\
rsp2.o


# the following for testing???
OOO= \
mrefl3.o	\
refcom.o	\
intarl.o	\
qsmnh.o \
ssube.o \
ssubi.o \
qsubs.o \
hr.o	\
refl.o	\
abscf.o		\
unmix.o	

HEADERS=convh.h  defs.h  lmrefl.h

dradtran:	$(OBJ) $(SP_LIB)/specpr.a $(SP_LIB)/sputil.a $(HEADERS)
	$(F77) $(SP_FFLAGS) $(LDFLAGS) -o dradtranx $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)
	mv dradtranx dradtran
	size dradtran

.SUFFIXES:
.SUFFIXES: .o .r

clean:	$(OBJ)
	rm $(OBJ)

radtran.o:		radtran.r $(HEADERS)
	$(SSPP) -r -$(SSPPFLAGS)  radtran.r > radtran.x
	$(RF) $(SP_RFLAGS) radtran.x > radtran.f
	$(F77) $(SP_FFLAGS) -c radtran.f
	rm -f radtran.x radtran.f

abscf.o:		abscf.r $(HEADERS)
	$(RF) $(SP_RFLAGS) abscf.r > abscf.f
	$(F77) $(SP_FFLAGS) -c abscf.f
	rm -f abscf.f

refcom.o:		refcom.r $(HEADERS)
	$(RF) $(SP_RFLAGS) refcom.r > refcom.f
	$(F77) $(SP_FFLAGS) -c refcom.f
	rm -f refcom.f

refmix.o:		refmix.r $(HEADERS)
	$(RF) $(SP_RFLAGS) refmix.r > refmix.f
	$(F77) $(SP_FFLAGS) -c refmix.f
	rm -f refmix.f

reflyrmix.o:		reflyrmix.r $(HEADERS)
	$(RF) $(SP_RFLAGS) reflyrmix.r > reflyrmix.f
	$(F77) $(SP_FFLAGS) -c reflyrmix.f
	rm -f reflyrmix.f

embedmix.o:		embedmix.r $(HEADERS)
	$(RF) $(SP_RFLAGS) embedmix.r > embedmix.f
	$(F77) $(SP_FFLAGS) -c embedmix.f
	rm -f embedmix.f

getns.o:		getns.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getns.r > getns.f
	$(F77) $(SP_FFLAGS) -c getns.f
	rm -f getns.f

getspec.o:		getspec.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getspec.r > getspec.f
	$(F77) $(SP_FFLAGS) -c getspec.f
	rm -f getspec.f

getdfp.o:		getdfp.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getdfp.r > getdfp.f
	$(F77) $(SP_FFLAGS) -c getdfp.f
	rm -f getdfp.f

getembn.o:		getembn.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getembn.r > getembn.f
	$(F77) $(SP_FFLAGS) -c getembn.f
	rm -f getembn.f

getembk.o:		getembk.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getembk.r > getembk.f
	$(F77) $(SP_FFLAGS) -c getembk.f
	rm -f getembk.f

getlayerthick.o:		getlayerthick.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getlayerthick.r > getlayerthick.f
	$(F77) $(SP_FFLAGS) -c getlayerthick.f
	rm -f getlayerthick.f

kweigh.o:		kweigh.r $(HEADERS)
	$(RF) $(SP_RFLAGS) kweigh.r > kweigh.f
	$(F77) $(SP_FFLAGS) -c kweigh.f
	rm -f kweigh.f

abpeak.o:		abpeak.r $(HEADERS)
	$(RF) $(SP_RFLAGS) abpeak.r > abpeak.f
	$(F77) $(SP_FFLAGS) -c abpeak.f
	rm -f abpeak.f

getmu.o:		getmu.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getmu.r > getmu.f
	$(F77) $(SP_FFLAGS) -c getmu.f
	rm -f getmu.f

calcr.o:		calcr.r $(HEADERS)
	$(RF) $(SP_RFLAGS) calcr.r > calcr.f
	$(F77) $(SP_FFLAGS) -c calcr.f
	rm -f calcr.f

calcrlyr.o:		calcrlyr.r $(HEADERS)
	$(RF) $(SP_RFLAGS) calcrlyr.r > calcrlyr.f
	$(F77) $(SP_FFLAGS) -c calcrlyr.f
	rm -f calcrlyr.f

cnvg1g.o:		cnvg1g.r $(HEADERS)
	$(RF) $(SP_RFLAGS) cnvg1g.r > cnvg1g.f
	$(F77) $(SP_FFLAGS) -c cnvg1g.f
	rm -f cnvg1g.f

cnvgbr.o:		cnvgbr.r $(HEADERS)
	$(RF) $(SP_RFLAGS) cnvgbr.r > cnvgbr.f
	$(F77) $(SP_FFLAGS) -c cnvgbr.f
	rm -f cnvgbr.f

nweigh.o:		nweigh.r $(HEADERS)
	$(RF) $(SP_RFLAGS) nweigh.r > nweigh.f
	$(F77) $(SP_FFLAGS) -c nweigh.f
	rm -f nweigh.f

kfind.o:		kfind.r $(HEADERS)
	$(RF) $(SP_RFLAGS) kfind.r > kfind.f
	$(F77) $(SP_FFLAGS) -c kfind.f
	rm -f kfind.f

spcwri.o:		spcwri.r $(HEADERS)
	$(RF) $(SP_RFLAGS) spcwri.r > spcwri.f
	$(F77) $(SP_FFLAGS) -c spcwri.f
	rm -f spcwri.f

spctwri.o:		spctwri.r $(HEADERS)
	$(RF) $(SP_RFLAGS) spctwri.r > spctwri.f
	$(F77) $(SP_FFLAGS) -c spctwri.f
	rm -f spctwri.f

wavavg.o:		wavavg.r $(HEADERS)
	$(RF) $(SP_RFLAGS) wavavg.r > wavavg.f
	$(F77) $(SP_FFLAGS) -c wavavg.f
	rm -f wavavg.f

nfind.o:		nfind.r $(HEADERS)
	$(RF) $(SP_RFLAGS) nfind.r > nfind.f
	$(F77) $(SP_FFLAGS) -c nfind.f
	rm -f nfind.f

kiter.o:		kiter.r $(HEADERS)
	$(RF) $(SP_RFLAGS) kiter.r > kiter.f
	$(F77) $(SP_FFLAGS) -c kiter.f
	rm -f kiter.f

plotit.o:		plotit.r $(HEADERS)
	$(RF) $(SP_RFLAGS) plotit.r > plotit.f
	$(F77) $(SP_FFLAGS) -c plotit.f
	rm -f plotit.f

getwav.o:		getwav.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getwav.r > getwav.f
	$(F77) $(SP_FFLAGS) -c getwav.f
	rm -f getwav.f

getnset.o:		getnset.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getnset.r > getnset.f
	$(F77) $(SP_FFLAGS) -c getnset.f
	rm -f getnset.f

dwcomp.o:		dwcomp.r $(HEADERS)
	$(RF) $(SP_RFLAGS) dwcomp.r > dwcomp.f
	$(F77) $(SP_FFLAGS) -c dwcomp.f
	rm -f dwcomp.f

getk.o:		getk.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getk.r > getk.f
	$(F77) $(SP_FFLAGS) -c getk.f
	rm -f getk.f

getn.o:		getn.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getn.r > getn.f
	$(F77) $(SP_FFLAGS) -c getn.f
	rm -f getn.f

getn2.o:		getn2.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getn2.r > getn2.f
	$(F77) $(SP_FFLAGS) -c getn2.f
	rm -f getn2.f

getk2.o:		getk2.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getk2.r > getk2.f
	$(F77) $(SP_FFLAGS) -c getk2.f
	rm -f getk2.f

getnst2.o:		getnst2.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getnst2.r > getnst2.f
	$(F77) $(SP_FFLAGS) -c getnst2.f
	rm -f getnst2.f

getdefs.o:		getdefs.r $(HEADERS)
	$(RF) $(SP_RFLAGS) getdefs.r > getdefs.f
	$(F77) $(SP_FFLAGS) -c getdefs.f
	rm -f getdefs.f

mrefl3.o:		mrefl3.r $(HEADERS)
	$(RF) $(SP_RFLAGS) mrefl3.r > mrefl3.f
	$(F77) $(SP_FFLAGS) -c mrefl3.f
	rm -f mrefl3.f

mrefl3lyr.o:		mrefl3lyr.r $(HEADERS)
	$(RF) $(SP_RFLAGS) mrefl3lyr.r > mrefl3lyr.f
	$(F77) $(SP_FFLAGS) -c mrefl3lyr.f
	rm -f mrefl3lyr.f

csatrd.o:		csatrd.r $(HEADERS)
	$(RF) $(SP_RFLAGS) csatrd.r > csatrd.f
	$(F77) $(SP_FFLAGS) -c csatrd.f
	rm -f csatrd.f

cxband.o:		cxband.r $(HEADERS)
	$(RF) $(SP_RFLAGS) cxband.r > cxband.f
	$(F77) $(SP_FFLAGS) -c cxband.f
	rm -f cxband.f

xccont.o:		xccont.r $(HEADERS)
	$(RF) $(SP_RFLAGS) xccont.r > xccont.f
	$(F77) $(SP_FFLAGS) -c xccont.f
	rm -f xccont.f

eval1g.o:		eval1g.r $(HEADERS)
	$(RF) $(SP_RFLAGS) eval1g.r > eval1g.f
	$(F77) $(SP_FFLAGS) -c eval1g.f
	rm -f eval1g.f

bdrefl.o:		bdrefl.r $(HEADERS)
	$(RF) $(SP_RFLAGS) bdrefl.r > bdrefl.f
	$(F77) $(SP_FFLAGS) -c bdrefl.f
	rm -f bdrefl.f

stdref.o:		stdref.r $(HEADERS)
	$(RF) $(SP_RFLAGS) stdref.r > stdref.f
	$(F77) $(SP_FFLAGS) -c stdref.f
	rm -f stdref.f

ssube.o:		ssube.r $(HEADERS)
	$(RF) $(SP_RFLAGS) ssube.r > ssube.f
	$(F77) $(SP_FFLAGS) -c ssube.f
	rm -f ssube.f

ssubi.o:		ssubi.r $(HEADERS)
	$(RF) $(SP_RFLAGS) ssubi.r > ssubi.f
	$(F77) $(SP_FFLAGS) -c ssubi.f
	rm -f ssubi.f

ssube1981.o:		ssube1981.f
	$(F77) $(SP_FFLAGS) -c ssube1981.f

ssubi1981.o:		ssubi1981.f
	$(F77) $(SP_FFLAGS) -c ssubi1981.f

qsmnh.o:		qsmnh.r
	$(RF) $(SP_RFLAGS) qsmnh.r > qsmnh.f
	$(F77) $(SP_FFLAGS) -c qsmnh.f
	rm -f qsmnh.f

qsubs.o:		qsubs.f
	$(F77) $(SP_FFLAGS) -c qsubs.f

singscat.o:		singscat.f
	$(F77) $(SP_FFLAGS) -c  singscat.f

hr.o:		hr.f
	$(F77) $(SP_FFLAGS) -c hr.f

refl.o:		refl.r
	$(RF) $(SP_RFLAGS) refl.r > refl.f
	$(F77) $(SP_FFLAGS) -c refl.f
	rm -f refl.f

unmix.o:		unmix.r $(HEADERS)
	$(RF) $(SP_RFLAGS) unmix.r > unmix.f
	$(F77) $(SP_FFLAGS) -c unmix.f
	rm -f unmix.f

f_Rpara.o:		f_Rpara.f
	$(F77) $(SP_FFLAGS) -c f_Rpara.f

f_Rperp.o:		f_Rperp.f
	$(F77) $(SP_FFLAGS) -c f_Rperp.f

f_Se_pg59.o:		f_Se_pg59.f
	$(F77) $(SP_FFLAGS) -c f_Se_pg59.f

f_Se_pg60.o:		f_Se_pg60.f
	$(F77) $(SP_FFLAGS) -c f_Se_pg60.f

rsp2.o:			rsp2.f
	$(F77) $(SP_FFLAGS) -c rsp2.f




install:
	cp -f dradtran $(SP_LOCAL)/bin/pradtran
	chmod 755 $(SP_LOCAL)/bin/pradtran

