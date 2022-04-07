# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
fnalg.o 	\
fnhist.o 	\
fnhypt.o 	\
fnlgs.o 	\
fnpowr.o 	\
fntrig.o 	\
wfcn.o 	\
wfcn1.o 

$(SP_OBJ)/talg.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
fnalg.o:		fnalg.r
	$(RF) $(SP_RFLAGS) fnalg.r > fnalg.f
	$(F77) $(SP_FFLAGS) -c fnalg.f
	rm -f fnalg.f


fnhist.o:		fnhist.r
	$(RF) $(SP_RFLAGS) fnhist.r > fnhist.f
	$(F77) $(SP_FFLAGS) -c fnhist.f
	rm -f fnhist.f


fnhypt.o:		fnhypt.r
	$(RF) $(SP_RFLAGS) fnhypt.r > fnhypt.f
	$(F77) $(SP_FFLAGS) -c fnhypt.f
	rm -f fnhypt.f


fnlgs.o:		fnlgs.r
	$(RF) $(SP_RFLAGS) fnlgs.r > fnlgs.f
	$(F77) $(SP_FFLAGS) -c fnlgs.f
	rm -f fnlgs.f


fnpowr.o:		fnpowr.r
	$(RF) $(SP_RFLAGS) fnpowr.r > fnpowr.f
	$(F77) $(SP_FFLAGS) -c fnpowr.f
	rm -f fnpowr.f


fntrig.o:		fntrig.r
	$(RF) $(SP_RFLAGS) fntrig.r > fntrig.f
	$(F77) $(SP_FFLAGS) -c fntrig.f
	rm -f fntrig.f


wfcn.o:		wfcn.r
	$(RF) $(SP_RFLAGS) wfcn.r > wfcn.f
	$(F77) $(SP_FFLAGS) -c wfcn.f
	rm -f wfcn.f


wfcn1.o:		wfcn1.r
	$(RF) $(SP_RFLAGS) wfcn1.r > wfcn1.f
	$(F77) $(SP_FFLAGS) -c wfcn1.f
	rm -f wfcn1.f

