# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
allist.o \
copyin.o \
crtin.o \
decod1.o \
getstr.o \
getkey.o \
ihchar.o \
chchar.o \
keytrn.o \
list.o 	\
prpixl.o \
refile.o \
rlchng.o \
setals.o \
setkey.o \
sphelp.o \
typein.o \
varsub.o \
varpar.o \
what.o \
wjfren.o \
cmdverbose.o \
getiopconchar.o \
xfer.o



$(SP_OBJ)/free.o:	$(OBJ)	   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $(SP_OBJ)/free.o $(OBJ)

copyin.o:		copyin.r
	$(RF) $(SP_RFLAGS) copyin.r > copyin.f
	$(F77) $(SP_FFLAGS) -c copyin.f
	rm -f copyin.f

crtin.o:		crtin.r
	$(RF) $(SP_RFLAGS) crtin.r > crtin.f
	$(F77) $(SP_FFLAGS) -c crtin.f
	rm -f crtin.f

decod1.o:		decod1.r
	$(RF) $(SP_RFLAGS) decod1.r > decod1.f
	$(F77) $(SP_FFLAGS) -c decod1.f
	rm -f decod1.f

getstr.o:		getstr.r
	$(RF) $(SP_RFLAGS) getstr.r > getstr.f
	$(F77) $(SP_FFLAGS) -c getstr.f
	rm -f getstr.f

ihchar.o:		ihchar.r
	$(SSPP) -r -$(SSPPFLAGS)  ihchar.r > ihchar.x
	$(RF) $(SP_RFLAGS) ihchar.x > ihchar.f
	$(F77) $(SP_FFLAGS) -c ihchar.f
	rm -f ihchar.x ihchar.f

chchar.o:		chchar.r
	$(SSPP) -r -$(SSPPFLAGS)  chchar.r > chchar.x
	$(RF) $(SP_RFLAGS) chchar.x > chchar.f
	$(F77) $(SP_FFLAGS) -c chchar.f
	rm -f chchar.x chchar.f

list.o:		list.r
	$(RF) $(SP_RFLAGS) list.r > list.f
	$(F77) $(SP_FFLAGS) -c list.f
	rm -f list.f

prpixl.o:		prpixl.r
	$(RF) $(SP_RFLAGS) prpixl.r > prpixl.f
	$(F77) $(SP_FFLAGS) -c prpixl.f
	rm -f prpixl.f

refile.o:		refile.r
	$(RF) $(SP_RFLAGS) refile.r > refile.f
	$(F77) $(SP_FFLAGS) -c refile.f
	rm -f refile.f

rlchng.o:		rlchng.r
	$(RF) $(SP_RFLAGS) rlchng.r > rlchng.f 
	$(F77) $(SP_FFLAGS) -c rlchng.f
	rm -f rlchng.f

typein.o:		typein.r
	$(RF) $(SP_RFLAGS) typein.r > typein.f
	$(F77) $(SP_FFLAGS) -c typein.f
	rm -f typein.f

varsub.o:		varsub.r
	$(RF) $(SP_RFLAGS) varsub.r > varsub.f
	$(F77) $(SP_FFLAGS) -c varsub.f
	rm -f varsub.f

varpar.o:		varpar.r
	$(RF) $(SP_RFLAGS) varpar.r > varpar.f
	$(F77) $(SP_FFLAGS) -c varpar.f
	rm -f varpar.f

what.o:		what.r
	$(RF) $(SP_RFLAGS) what.r > what.f
	$(F77) $(SP_FFLAGS) -c what.f
	rm -f what.f

wjfren.o:		wjfren.r
	$(RF) $(SP_RFLAGS) wjfren.r > wjfren.f
	$(F77) $(SP_FFLAGS) -c wjfren.f
	rm -f wjfren.f

xfer.o:		xfer.r
	$(RF) $(SP_RFLAGS) xfer.r > xfer.f
	$(F77) $(SP_FFLAGS) -c xfer.f
	rm -f xfer.f

allist.o:		allist.r
	$(RF) $(SP_RFLAGS) allist.r > allist.f
	$(F77) $(SP_FFLAGS) -c allist.f
	rm -f allist.f

getkey.o:		getkey.r
	$(RF) $(SP_RFLAGS) getkey.r > getkey.f
	$(F77) $(SP_FFLAGS) -c getkey.f
	rm -f getkey.f

keytrn.o:		keytrn.r
	$(RF) $(SP_RFLAGS) keytrn.r > keytrn.f
	$(F77) $(SP_FFLAGS) -c keytrn.f
	rm -f keytrn.f

setals.o:		setals.r
	$(RF) $(SP_RFLAGS) setals.r > setals.f
	$(F77) $(SP_FFLAGS) -c setals.f
	rm -f setals.f

setkey.o:		setkey.r
	$(RF) $(SP_RFLAGS) setkey.r > setkey.f
	$(F77) $(SP_FFLAGS) -c setkey.f
	rm -f setkey.f

sphelp.o:		sphelp.r
	$(RF) $(SP_RFLAGS) sphelp.r > sphelp.f
	$(F77) $(SP_FFLAGS) -c sphelp.f
	rm -f sphelp.f

cmdverbose.o:		cmdverbose.r
	$(RF) $(SP_RFLAGS) cmdverbose.r > cmdverbose.f
	$(F77) $(SP_FFLAGS) -c cmdverbose.f
	rm -f cmdverbose.f

getiopconchar.o:		getiopconchar.r
	$(RF) $(SP_RFLAGS) getiopconchar.r > getiopconchar.f
	$(F77) $(SP_FFLAGS) -c getiopconchar.f
	rm -f getiopconchar.f

