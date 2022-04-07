# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ= \
alplty.o \
chplta.o \
crtplt.o \
crtpsc.o \
er.o \
cursrd.o \
gcrpos.o \
gcchan.o \
in.o \
inice.o \
iwplta.o \
ma.o \
maw.o \
signif.o \
ticks.o \
va.o \
vaw.o \
pltbox1.o \
pltline.o \
comp3ptbd.o \
comptetf.o  \
pmenus1.o  \
shomenu1.o  \
defowpt.o  \
window.o \
wriout.o \
wvplta.o 

$(SP_OBJ)/crtp.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)


alplty.o:		alplty.r
	$(RF) $(SP_RFLAGS) alplty.r >  alplty.f 
	$(F77) $(SP_FFLAGS) -c alplty.f
	rm -f alplty.f

chplta.o:		chplta.r
	$(RF) $(SP_RFLAGS) chplta.r >  chplta.f
	$(F77) $(SP_FFLAGS) -c chplta.f
	rm -f chplta.f

crtplt.o:		crtplt.r
	$(RF) $(SP_RFLAGS) crtplt.r >  crtplt.f 
	$(F77) $(SP_FFLAGS) -c crtplt.f
	rm -f crtplt.f

pltbox1.o:		pltbox1.r
	$(RF) $(SP_RFLAGS) pltbox1.r >  pltbox1.f 
	$(F77) $(SP_FFLAGS) -c pltbox1.f
	rm -f pltbox1.f

pltline.o:		pltline.r
	$(RF) $(SP_RFLAGS) pltline.r >  pltline.f 
	$(F77) $(SP_FFLAGS) -c pltline.f
	rm -f pltline.f

comp3ptbd.o:		comp3ptbd.r
	$(RF) $(SP_RFLAGS) comp3ptbd.r > comp3ptbd.f 
	$(F77) $(SP_FFLAGS) -c comp3ptbd.f
	rm -f comp3ptbd.f

comptetf.o:		comptetf.r
	$(RF) $(SP_RFLAGS) comptetf.r > comptetf.f 
	$(F77) $(SP_FFLAGS) -c comptetf.f
	rm -f comptetf.f

pmenus1.o:		pmenus1.r
	$(RF) $(SP_RFLAGS) pmenus1.r > pmenus1.f 
	$(F77) $(SP_FFLAGS) -c pmenus1.f
	rm -f pmenus1.f

shomenu1.o:		shomenu1.r
	$(RF) $(SP_RFLAGS) shomenu1.r > shomenu1.f 
	$(F77) $(SP_FFLAGS) -c shomenu1.f
	rm -f shomenu1.f

defowpt.o:		defowpt.r
	$(RF) $(SP_RFLAGS) defowpt.r > defowpt.f 
	$(F77) $(SP_FFLAGS) -c defowpt.f
	rm -f defowpt.f

crtpsc.o:		crtpsc.r
	$(RF) $(SP_RFLAGS) crtpsc.r >  crtpsc.f 
	$(F77) $(SP_FFLAGS) -c  crtpsc.f
#	rm -f crtpsc.f

er.o:		er.r
	$(RF) $(SP_RFLAGS) er.r >  er.f 
	$(F77) $(SP_FFLAGS) -c er.f
	rm -f er.f

cursrd.o:		cursrd.r
	$(SSPP) -r -$(SSPPFLAGS) cursrd.r > cursrd.x
	$(RF) $(SP_RFLAGS) cursrd.x >  cursrd.f 
	$(F77) $(SP_FFLAGS) -c cursrd.f
	rm -f cursrd.x cursrd.f

gcchan.o:		gcchan.r
	$(RF) $(SP_RFLAGS) gcchan.r > gcchan.f 
	$(F77) $(SP_FFLAGS) -c gcchan.f
#	rm -f gcchan.f

gcrpos.o:		gcrpos.r
	$(RF) $(SP_RFLAGS) gcrpos.r > gcrpos.f 
	$(F77) $(SP_FFLAGS) -c gcrpos.f
#	rm -f gcrpos.f

in.o:		in.r
	$(RF) $(SP_RFLAGS) in.r >  in.f 
	$(F77) $(SP_FFLAGS) -c in.f
	rm -f in.f

inice.o:		inice.r
	$(RF) $(SP_RFLAGS) inice.r > inice.f 
	$(F77) $(SP_FFLAGS) -c inice.f
	rm -f inice.f

iwplta.o:		iwplta.r
	$(SSPP) -r -$(SSPPFLAGS) iwplta.r > iwplta.x
	$(RF) $(SP_RFLAGS) iwplta.x >  iwplta.f 
	$(F77) $(SP_FFLAGS) -c iwplta.f
	rm -f iwplta.f iwplta.x

ma.o:		ma.r
	$(RF) $(SP_RFLAGS) ma.r >  ma.f  
	$(F77) $(SP_FFLAGS) -c ma.f
	rm -f ma.f

maw.o:		maw.r
	$(RF) $(SP_RFLAGS) maw.r >  maw.f 
	$(F77) $(SP_FFLAGS) -c maw.f
	rm -f maw.f

signif.o:		signif.r
	$(RF) $(SP_RFLAGS) signif.r > signif.f 
	$(F77) $(SP_FFLAGS) -c signif.f
	rm -f signif.f

ticks.o:		ticks.r
	$(RF) $(SP_RFLAGS) ticks.r >  ticks.f
	$(F77) $(SP_FFLAGS) -c ticks.f
	rm -f ticks.f

va.o:		va.r
	$(RF) $(SP_RFLAGS) va.r >  va.f 
	$(F77) $(SP_FFLAGS) -c va.f
	rm -f va.f

vaw.o:		vaw.r
	$(RF) $(SP_RFLAGS) vaw.r >  vaw.f 
	$(F77) $(SP_FFLAGS) -c vaw.f
	rm -f vaw.f

window.o:		window.r
	$(RF) $(SP_RFLAGS) window.r >  window.f 
	$(F77) $(SP_FFLAGS) -c window.f
	rm -f window.f

wriout.o:		wriout.r
	$(SSPP) -r -$(SSPPFLAGS) wriout.r > wriout.x 
	$(RF) $(SP_RFLAGS) wriout.x >  wriout.f 
	$(F77) $(SP_FFLAGS) -c wriout.f
	rm -f wriout.x wriout.f

wvplta.o:		wvplta.r
	$(SSPP) -r -$(SSPPFLAGS) wvplta.r > wvplta.x
	$(RF) $(SP_RFLAGS) wvplta.x >  wvplta.f 
	$(F77) $(SP_FFLAGS) -c  wvplta.f
	rm -f wvplta.f wvplta.x
