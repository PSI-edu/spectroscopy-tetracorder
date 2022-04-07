# @(#) makefile 2.12@(#)

SHELL=/bin/sh
GET=sp_if_get

OBJ= \
lprint.o \
pdatl1.o \
ptxt1.o	\
ptxtc.o \
prheader.o \
zeroin.o \
cprint.o \
flname.o \
wcomma.o \
disv.o \
inftrn.o \
listfl.o \
startran.o \
tapevol.o \
tapelun.o \
tcheck.o \
redcod.o \
search.o \
getoverly.o \
getwavmark.o \
get3ptbd.o \
gettetf.o  \
fregex.o

dtol.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $(SP_OBJ)/dtol.o $(OBJ)

disv.o:		disv.r
	$(RF) $(SP_RFLAGS) disv.r > disv.f 
	$(F77) $(SP_FFLAGS) -c disv.f
	rm -f disv.f

inftrn.o:		inftrn.r
	$(RF) $(SP_RFLAGS) inftrn.r > inftrn.f 
	$(F77) $(SP_FFLAGS) -c inftrn.f
	rm -f inftrn.f

listfl.o:		listfl.r
	$(RF) $(SP_RFLAGS) listfl.r > listfl.f
	$(F77) $(SP_FFLAGS) -c listfl.f
	rm -f listfl.f

startran.o:		startran.r
	$(RF) $(SP_RFLAGS) startran.r > startran.f  
	$(F77) $(SP_FFLAGS) -c startran.f
	rm -f startran.f

tapevol.o:		tapevol.r
	$(RF) $(SP_RFLAGS) tapevol.r > tapevol.f 
	$(F77) $(SP_FFLAGS) -c tapevol.f
	rm -f tapevol.f

tapelun.o:		tapelun.r
	$(RF) $(SP_RFLAGS) tapelun.r > tapelun.f 
	$(F77) $(SP_FFLAGS) -c tapelun.f
	rm -f tapelun.f

tcheck.o:		tcheck.r
	$(RF) $(SP_RFLAGS) tcheck.r > tcheck.f
	$(F77) $(SP_FFLAGS) -c tcheck.f
	rm -f tcheck.f

redcod.o:		redcod.r
	$(RF) $(SP_RFLAGS) redcod.r > redcod.f
	$(F77) $(SP_FFLAGS) -c redcod.f
	rm -f redcod.f

search.o:		search.r
	$(SSPP) -r -$(SSPPFLAGS) search.r  > search.x
	$(RF) $(SP_RFLAGS) search.x > search.f
	$(F77) $(SP_FFLAGS) -c search.f
	rm -f search.x search.f

fregex.o:		fregex.cc
	$(SSPP) -c -$(SSPPFLAGS) fregex.cc > fregex.c
	$(CC) $(SP_CFLAGS) -c fregex.c
	rm -f fregex.c

lprint.o:		lprint.r
	$(RF) $(SP_RFLAGS) lprint.r > lprint.f 
	$(F77) $(SP_FFLAGS) -c lprint.f
	rm -f lprint.f

ptxt1.o:		ptxt1.r
	$(RF) $(SP_RFLAGS) ptxt1.r > ptxt1.f 
	$(F77) $(SP_FFLAGS) -c ptxt1.f
	rm -f ptxt1.f

pdatl1.o:		pdatl1.r
	$(RF) $(SP_RFLAGS) pdatl1.r > pdatl1.f 
	$(F77) $(SP_FFLAGS) -c pdatl1.f
	rm -f pdatl1.f

zeroin.o:		zeroin.r
	$(RF) $(SP_RFLAGS) zeroin.r > zeroin.f 
	$(F77) $(SP_FFLAGS) -c zeroin.f
	rm -f zeroin.f

cprint.o:		cprint.r
	$(RF) $(SP_RFLAGS) cprint.r > cprint.f 
	$(F77) $(SP_FFLAGS) -c cprint.f
	rm -f cprint.f

flname.o:		flname.r
	$(RF) $(SP_RFLAGS) flname.r > flname.f 
	$(F77) $(SP_FFLAGS) -c flname.f
	rm -f flname.f

wcomma.o:		wcomma.r
	$(RF) $(SP_RFLAGS) wcomma.r > wcomma.f 
	$(F77) $(SP_FFLAGS) -c wcomma.f
	rm -f wcomma.f

ptxtc.o:		ptxtc.r
	$(RF) $(SP_RFLAGS) ptxtc.r > ptxtc.f 
	$(F77) $(SP_FFLAGS) -c ptxtc.f
	rm -f ptxtc.f

getoverly.o:		getoverly.r
	$(RF) $(SP_RFLAGS) getoverly.r > getoverly.f 
	$(F77) $(SP_FFLAGS) -c getoverly.f
	rm -f getoverly.f

getwavmark.o:		getwavmark.r
	$(RF) $(SP_RFLAGS) getwavmark.r > getwavmark.f 
	$(F77) $(SP_FFLAGS) -c getwavmark.f
	rm -f getwavmark.f

get3ptbd.o:		get3ptbd.r
	$(RF) $(SP_RFLAGS) get3ptbd.r > get3ptbd.f 
	$(F77) $(SP_FFLAGS) -c get3ptbd.f
	rm -f get3ptbd.f

gettetf.o:		gettetf.r
	$(RF) $(SP_RFLAGS) gettetf.r > gettetf.f 
	$(F77) $(SP_FFLAGS) -c gettetf.f
	rm -f gettetf.f

prheader.o:		prheader.r
	$(RF) $(SP_RFLAGS) prheader.r > prheader.f 
	$(F77) $(SP_FFLAGS) -c prheader.f
	rm -f prheader.f

listfl.o redcod.o search.o:	srchdefs.h

