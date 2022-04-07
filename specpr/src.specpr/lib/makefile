# @(#) makefile 2.7@(#)

SHELL=/bin/sh

OBJ= 	\
dumpspool.o 	\
endnam.o 	\
filid.o 	\
filidw.o 	\
lpcrtp.o 	\
lprpct.o 	\
pdata.o 	\
plpage.o 	\
setspool.o 	\
unglic.o 	\
whedr.o 	\
dltpts.o	\
whedr2.o 	\
frdms.o		\
frjuld.o	\
todms.o		\
tojuld.o	\
jdatim.o	\
sptime.o	\
sptimb.o	\
crindex.o

$(SP_OBJ)/lib.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
dumpspool.o:		dumpspool.r
	$(RF) $(SP_RFLAGS) dumpspool.r > dumpspool.f
	$(F77) $(SP_FFLAGS) -c dumpspool.f
	rm -f dumpspool.f

endnam.o:		endnam.r
	$(RF) $(SP_RFLAGS) endnam.r > endnam.f
	$(F77) $(SP_FFLAGS) -c endnam.f
	rm -f endnam.f

filid.o:		filid.r
	$(RF) $(SP_RFLAGS) filid.r > filid.f
	$(F77) $(SP_FFLAGS) -c filid.f
	rm -f filid.f

filidw.o:		filidw.r
	$(RF) $(SP_RFLAGS) filidw.r > filidw.f
	$(F77) $(SP_FFLAGS) -c filidw.f
	rm -f filidw.f

lpcrtp.o:		lpcrtp.r
	$(RF) $(SP_RFLAGS) lpcrtp.r > lpcrtp.f
	$(F77) $(SP_FFLAGS) -c lpcrtp.f
	rm -f lpcrtp.f

lprpct.o:		lprpct.r
	$(RF) $(SP_RFLAGS) lprpct.r > lprpct.f
	$(F77) $(SP_FFLAGS) -c lprpct.f
	rm -f lprpct.f

pdata.o:		pdata.r
	$(RF) $(SP_RFLAGS) pdata.r > pdata.f
	$(F77) $(SP_FFLAGS) -c pdata.f
	rm -f pdata.f

plpage.o:		plpage.r
	$(RF) $(SP_RFLAGS) plpage.r > plpage.f
	$(F77) $(SP_FFLAGS) -c plpage.f
	rm -f plpage.f

setspool.o:		setspool.r
	$(RF) $(SP_RFLAGS) setspool.r > setspool.f
	$(F77) $(SP_FFLAGS) -c setspool.f
	rm -f setspool.f

unglic.o:		unglic.r
	$(RF) $(SP_RFLAGS) unglic.r > unglic.f
	$(F77) $(SP_FFLAGS) -c unglic.f
	rm -f unglic.f

whedr.o:		whedr.r
	$(RF) $(SP_RFLAGS) whedr.r > whedr.f
	$(F77) $(SP_FFLAGS) -c whedr.f
	rm -f whedr.f

dltpts.o:		dltpts.r
	$(RF) $(SP_RFLAGS) dltpts.r > dltpts.f
	$(F77) $(SP_FFLAGS) -c dltpts.f
	rm -f dltpts.f

whedr2.o:		whedr2.r
	$(RF) $(SP_RFLAGS) whedr2.r > whedr2.f
	$(F77) $(SP_FFLAGS) -c whedr2.f
	rm -f whedr2.f

crindex.o:		crindex.r
	$(RF) $(SP_RFLAGS) crindex.r > crindex.f
	$(F77) $(SP_FFLAGS) -c crindex.f
	rm -f crindex.f

frjuld.o:		frjuld.r
	$(RF) $(SP_RFLAGS) frjuld.r > frjuld.f
	$(F77) $(SP_FFLAGS) -c frjuld.f
	rm -f frjuld.f

frdms.o:		frdms.r
	$(RF) $(SP_RFLAGS) frdms.r > frdms.f
	$(F77) $(SP_FFLAGS) -c frdms.f
	rm -f frdms.f

todms.o:		todms.r
	$(RF) $(SP_RFLAGS) todms.r > todms.f
	$(F77) $(SP_FFLAGS) -c todms.f
	rm -f todms.f

tojuld.o:		tojuld.r
	$(RF) $(SP_RFLAGS) tojuld.r > tojuld.f
	$(F77) $(SP_FFLAGS) -c tojuld.f
	rm -f tojuld.f

jdatim.o:		jdatim.r
	$(RF) $(SP_RFLAGS) jdatim.r > jdatim.f
	$(F77) $(SP_FFLAGS) -c jdatim.f
	rm -f jdatim.f

sptime.o:		sptime.r
	$(RF) $(SP_RFLAGS) sptime.r > sptime.f
	$(F77) $(SP_FFLAGS) -c sptime.f
	rm -f sptime.f

sptimb.o:		sptimb.r
	$(RF) $(SP_RFLAGS) sptimb.r > sptimb.f
	$(F77) $(SP_FFLAGS) -c sptimb.f
	rm -f sptimb.f
