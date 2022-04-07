# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=		\
namef.o 	\
wdgsb1.o 	\
wdgsb2.o 	\
wdgsb3.o 	\
wdgsb4.o 	\
wdgsb5.o 	\
wedgea.o 

$(SP_OBJ)/init.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $(SP_OBJ)/init.o $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
namef.o:		namef.r
	$(RF) $(SP_RFLAGS) namef.r > namef.f
	$(F77) $(SP_FFLAGS) -c namef.f
	rm -f namef.f

wdgsb1.o:		wdgsb1.r
	$(RF) $(SP_RFLAGS) wdgsb1.r > wdgsb1.f
	$(F77) $(SP_FFLAGS) -c wdgsb1.f
	rm -f wdgsb1.f

wdgsb2.o:		wdgsb2.r
	$(RF) $(SP_RFLAGS) wdgsb2.r > wdgsb2.f
	$(F77) $(SP_FFLAGS) -c wdgsb2.f
	rm -f wdgsb2.f

wdgsb3.o:		wdgsb3.r
	$(RF) $(SP_RFLAGS) wdgsb3.r > wdgsb3.f
	$(F77) $(SP_FFLAGS) -c wdgsb3.f
	rm -f wdgsb3.f

wdgsb4.o:		wdgsb4.r
	$(RF) $(SP_RFLAGS) wdgsb4.r > wdgsb4.f
	$(F77) $(SP_FFLAGS) -c wdgsb4.f
	rm -f wdgsb4.f

wdgsb5.o:		wdgsb5.r
	$(RF) $(SP_RFLAGS) wdgsb5.r > wdgsb5.f
	$(F77) $(SP_FFLAGS) -c wdgsb5.f
	rm -f wdgsb5.f

wedgea.o:		wedgea.r
	$(RF) $(SP_RFLAGS) wedgea.r > wedgea.f
	$(F77) $(SP_FFLAGS) -c wedgea.f
	rm -f wedgea.f
