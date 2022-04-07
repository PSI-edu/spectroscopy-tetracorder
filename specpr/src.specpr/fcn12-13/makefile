# @(#) makefile 2.5@(#)

SHELL=/bin/sh

OBJ=	\
bubble.o 	\
f12.o 	\
f12itp.o 	\
f12wav.o 	\
f13.o		\
icsicu.o	\
icsevu.o	\
merger.o

$(SP_OBJ)/fcn12-13.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

bubble.o:		bubble.r
	$(RF) $(SP_RFLAGS) bubble.r > bubble.f 
	$(F77) $(SP_FFLAGS) -c bubble.f
	rm -f bubble.f

f12.o:		f12.r
	$(RF) $(SP_RFLAGS) f12.r > f12.f 
	$(F77) $(SP_FFLAGS) -c f12.f
	rm -f f12.f

f12itp.o:		f12itp.r
	$(RF) $(SP_RFLAGS) f12itp.r > f12itp.f
	$(F77) $(SP_FFLAGS) -c f12itp.f
	rm -f f12itp.f

f12wav.o:		f12wav.r
	$(RF) $(SP_RFLAGS) f12wav.r > f12wav.f 
	$(F77) $(SP_FFLAGS) -c f12wav.f
	rm -f f12wav.f

f13.o:		f13.r
	$(RF) $(SP_RFLAGS) f13.r > f13.f
	$(F77) $(SP_FFLAGS) -c f13.f
	rm -f f13.f

icsicu.o:		icsicu.r
	$(RF) $(SP_RFLAGS) icsicu.r > icsicu.f 
	$(F77) $(SP_FFLAGS) -c icsicu.f
	rm -f icsicu.f

icsevu.o:		icsevu.r
	$(RF) $(SP_RFLAGS) icsevu.r > icsevu.f
	$(F77) $(SP_FFLAGS) -c icsevu.f
	rm -f icsevu.f

merger.o:		merger.r
	$(RF) $(SP_RFLAGS) merger.r > merger.f
	$(F77) $(SP_FFLAGS) -c merger.f
	rm -f merger.f
