# @(#) makefile 2.8@(#)

SHELL=/bin/sh

OBJ=	\
closnr.o	\
bytorder.o	\
devlun.o 	\
lundev.o 	\
devok.o 	\
devol.o 	\
devsta.o 	\
erored.o 	\
ertyp.o 	\
finfil.o 	\
newsta.o 	\
posfil.o 	\
reastr.o 	\
rederr.o	\
redfil.o 	\
wrifil.o 	\
wristr.o 	\
fpt2v.o		\
fpv2t.o		\
xfti.o		\
x2ti.o		\
pad.o		\
swab4order.o	\
swap.o


$(SP_OBJ)/io.o:		$(OBJ)	   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r .c
	
closnr.o:		closnr.r
	$(RF) $(SP_RFLAGS) closnr.r > closnr.f
	$(F77) $(SP_FFLAGS) -c closnr.f
	rm -f closnr.f

bytorder.o:		bytorder.r
	$(RF) $(SP_RFLAGS) bytorder.r > bytorder.f
	$(F77) $(SP_FFLAGS) -c bytorder.f
	rm -f bytorder.f

swab4order.o:		swab4order.c
	$(CC) $(SP_CFLAGS) -c swab4order.c

lundev.o:		lundev.r
	$(RF) $(SP_RFLAGS) lundev.r > lundev.f
	$(F77) $(SP_FFLAGS) -c lundev.f
	rm -f lundev.f

devlun.o:		devlun.r
	$(RF) $(SP_RFLAGS) devlun.r > devlun.f
	$(F77) $(SP_FFLAGS) -c devlun.f
	rm -f devlun.f

devok.o:		devok.r
	$(RF) $(SP_RFLAGS) devok.r > devok.f
	$(F77) $(SP_FFLAGS) -c devok.f
	rm -f devok.f

devol.o:		devol.r
	$(RF) $(SP_RFLAGS) devol.r > devol.f
	$(F77) $(SP_FFLAGS) -c devol.f
	rm -f devol.f

devsta.o:		devsta.r
	$(RF) $(SP_RFLAGS) devsta.r > devsta.f
	$(F77) $(SP_FFLAGS) -c devsta.f
	rm -f devsta.f

erored.o:		erored.r
	$(RF) $(SP_RFLAGS) erored.r > erored.f
	$(F77) $(SP_FFLAGS) -c erored.f
	rm -f erored.f

ertyp.o:		ertyp.r
	$(RF) $(SP_RFLAGS) ertyp.r > ertyp.f
	$(F77) $(SP_FFLAGS) -c ertyp.f
	rm -f ertyp.f

finfil.o:		finfil.r
	$(RF) $(SP_RFLAGS) finfil.r > finfil.f
	$(F77) $(SP_FFLAGS) -c finfil.f
	rm -f finfil.f

newsta.o:		newsta.r
	$(RF) $(SP_RFLAGS) newsta.r > newsta.f
	$(F77) $(SP_FFLAGS) -c newsta.f
	rm -f newsta.f

posfil.o:		posfil.r
	$(RF) $(SP_RFLAGS) posfil.r > posfil.f
	$(F77) $(SP_FFLAGS) -c posfil.f
	rm -f posfil.f

reastr.o:		reastr.r
	$(RF) $(SP_RFLAGS) reastr.r > reastr.f
	$(F77) $(SP_FFLAGS) -c reastr.f
	rm -f reastr.f

rederr.o:		rederr.r
	$(RF) $(SP_RFLAGS) rederr.r > rederr.f
	$(F77) $(SP_FFLAGS) -c rederr.f
	rm -f rederr.f

redfil.o:		redfil.r
	$(SSPP) -r -$(SSPPFLAGS) redfil.r > redfil.x
	$(RF) $(SP_RFLAGS) redfil.x > redfil.f
	$(F77) $(SP_FFLAGS) -c redfil.f
	rm -f redfil.x redfil.f

wrifil.o:		wrifil.r
	$(SSPP) -r -$(SSPPFLAGS) wrifil.r > wrifil.x
	$(RF) $(SP_RFLAGS) wrifil.x > wrifil.f
	$(F77) $(SP_FFLAGS) -c wrifil.f
	rm -f wrifil.x wrifil.f

wristr.o:		wristr.r
	$(RF) $(SP_RFLAGS) wristr.r > wristr.f
	$(F77) $(SP_FFLAGS) -c wristr.f
	rm -f wristr.f

fpt2v.o:		fpt2v.c
	$(CC) $(SP_CFLAGS) -c fpt2v.c

fpv2t.o:		fpv2t.c
	$(CC) $(SP_CFLAGS) -c fpv2t.c

xfti.o:		xfti.r
	$(RF) $(SP_RFLAGS) xfti.r > xfti.f
	$(F77) $(SP_FFLAGS) -c xfti.f
	rm -f xfti.f

x2ti.o:		x2ti.r
	$(RF) $(SP_RFLAGS) x2ti.r > x2ti.f
	$(F77) $(SP_FFLAGS) -c x2ti.f
	rm -f x2ti.f

pad.o:		pad.r
	$(RF) $(SP_RFLAGS) pad.r > pad.f
	$(F77) $(SP_FFLAGS) -c pad.f
	rm -f pad.f

swap.o:		swap.c
	$(CC)  $(SP_CFLAGS) -c swap.c
