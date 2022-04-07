# @(#) makefile 2.6@(#)

SHELL=/bin/sh

OBJ=	\
chgin1.o	\
chgin2.o 	\
chgin3.o 	\
chgin4.o	\
chginf.o 	\
chgtxt.o 	\
chinfo.o 	\
dtchng.o 	\
getdt.o 	\
getime.o 	\
getang.o	\
initdt.o	\
intchg.o 	\
redtxt.o	\
scrn00.o 	\
scrn01.o 	\
scrn02.o	\
scrn03.o 	\
timchg.o

$(SP_OBJ)/hed.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
chgin1.o:		chgin1.r
	$(RF) $(SP_RFLAGS) chgin1.r > chgin1.f
	$(F77) $(SP_FFLAGS) -c chgin1.f
	rm -f chgin1.f

chgin2.o:		chgin2.r
	$(RF) $(SP_RFLAGS) chgin2.r > chgin2.f
	$(F77) $(SP_FFLAGS) -c chgin2.f
	rm -f chgin2.f

chgin3.o:		chgin3.r
	$(RF) $(SP_RFLAGS) chgin3.r > chgin3.f
	$(F77) $(SP_FFLAGS) -c chgin3.f
	rm -f chgin3.f

chgin4.o:		chgin4.r
	$(RF) $(SP_RFLAGS) chgin4.r > chgin4.f
	$(F77) $(SP_FFLAGS) -c chgin4.f
	rm -f chgin4.f

chginf.o:		chginf.r
	$(RF) $(SP_RFLAGS) chginf.r > chginf.f
	$(F77) $(SP_FFLAGS) -c chginf.f
	rm -f chginf.f

chgtxt.o:		chgtxt.r
	$(RF) $(SP_RFLAGS) chgtxt.r > chgtxt.f
	$(F77) $(SP_FFLAGS) -c chgtxt.f
	rm -f chgtxt.f

chinfo.o:		chinfo.r
	$(RF) $(SP_RFLAGS) chinfo.r > chinfo.f
	$(F77) $(SP_FFLAGS) -c chinfo.f
	rm -f chinfo.f

dtchng.o:		dtchng.r
	$(RF) $(SP_RFLAGS) dtchng.r > dtchng.f
	$(F77) $(SP_FFLAGS) -c dtchng.f
	rm -f dtchng.f

getang.o:		getang.r
	$(RF) $(SP_RFLAGS) getang.r > getang.f
	$(F77) $(SP_FFLAGS) -c getang.f
	rm -f getang.f

getdt.o:		getdt.r
	$(RF) $(SP_RFLAGS) getdt.r > getdt.f
	$(F77) $(SP_FFLAGS) -c getdt.f
	rm -f getdt.f

getime.o:		getime.r
	$(RF) $(SP_RFLAGS) getime.r > getime.f
	$(F77) $(SP_FFLAGS) -c getime.f
	rm -f getime.f

initdt.o:		initdt.r
	$(RF) $(SP_RFLAGS) initdt.r > initdt.f
	$(F77) $(SP_FFLAGS) -c initdt.f
	rm -f initdt.f

intchg.o:		intchg.r
	$(RF) $(SP_RFLAGS) intchg.r > intchg.f
	$(F77) $(SP_FFLAGS) -c intchg.f
	rm -f intchg.f

redtxt.o:		redtxt.r
	$(RF) $(SP_RFLAGS) redtxt.r > redtxt.f
	$(F77) $(SP_FFLAGS) -c redtxt.f
	rm -f redtxt.f

scrn00.o:		scrn00.r
	$(RF) $(SP_RFLAGS) scrn00.r > scrn00.f
	$(F77) $(SP_FFLAGS) -c scrn00.f
	rm -f scrn00.f

scrn01.o:		scrn01.r
	$(RF) $(SP_RFLAGS) scrn01.r > scrn01.f
	$(F77) $(SP_FFLAGS) -c scrn01.f
	rm -f scrn01.f

scrn02.o:		scrn02.r
	$(RF) $(SP_RFLAGS) scrn02.r > scrn02.f
	$(F77) $(SP_FFLAGS) -c scrn02.f
	rm -f scrn02.f

scrn03.o:		scrn03.r
	$(RF) $(SP_RFLAGS) scrn03.r > scrn03.f
	$(F77) $(SP_FFLAGS) -c scrn03.f
	rm -f scrn03.f

timchg.o:		timchg.r
	$(RF) $(SP_RFLAGS) timchg.r > timchg.f
	$(F77) $(SP_FFLAGS) -c timchg.f
	rm -f timchg.f
