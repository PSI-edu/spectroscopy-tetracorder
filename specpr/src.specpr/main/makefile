# @(#) makefile 2.9@(#)

SHELL=/bin/sh

OBJ= \
closef.o 	\
callgfit.o		\
getcmdargs.o	\
mninst.o 	\
prochk.o 	\
reddisv.o 	\
e1trap.o	\
taprw.o 

all:	$(SP_OBJ)/main.o specpr.o

$(SP_OBJ)/main.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@  $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r

specpr.o:	specpr.r
	$(SSPP) -r -$(SSPPFLAGS) specpr.r > specpr.x
	$(RF) $(SP_RFLAGS) specpr.x > specpr.f
	$(F77) $(SP_FFLAGS) -c specpr.f
	rm -f specpr.x specpr.f
	
closef.o:		closef.r
	$(RF) $(SP_RFLAGS) closef.r > closef.f
	$(F77) $(SP_FFLAGS) -c closef.f
	rm -f closef.f

callgfit.o:		callgfit.r
	$(RF) $(SP_RFLAGS) callgfit.r > callgfit.f
	$(F77) $(SP_FFLAGS) -c callgfit.f
	rm -f callgfit.f

getcmdargs.o:		getcmdargs.r
	$(SSPP) -r -$(SSPPFLAGS) getcmdargs.r > getcmdargs.x
	$(RF) $(SP_RFLAGS) getcmdargs.x > getcmdargs.f
	$(F77) $(SP_FFLAGS) -c getcmdargs.f
	rm -f getcmdargs.x getcmdargs.f

mninst.o:		mninst.r
	$(RF) $(SP_RFLAGS) mninst.r > mninst.f
	$(F77) $(SP_FFLAGS) -c mninst.f
	rm -f mninst.f

prochk.o:		prochk.r
	$(RF) $(SP_RFLAGS) prochk.r > prochk.f
	$(F77) $(SP_FFLAGS) -c prochk.f
	rm -f prochk.f

reddisv.o:		reddisv.r
	$(RF) $(SP_RFLAGS) reddisv.r > reddisv.f
	$(F77) $(SP_FFLAGS) -c reddisv.f
	rm -f reddisv.f

taprw.o:		taprw.r
	$(RF) $(SP_RFLAGS) taprw.r > taprw.f
	$(F77) $(SP_FFLAGS) -c taprw.f
	rm -f taprw.f

e1trap.o:		e1trap.r
	$(RF) $(SP_RFLAGS) e1trap.r > e1trap.f
	$(F77) $(SP_FFLAGS) -c e1trap.f
	rm -f e1trap.f
