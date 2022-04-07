# @(#) makefile 2.1@(#)

SHELL=/bin/sh

OBJ=	\
f26.o	\
f27.o	\
f28.o	\
f29.o	

$(SP_OBJ)/dummy.o:	$(OBJ)  ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
f26.o	work/f26.r:
	$(RF) $(SP_RFLAGS) work/f26.r >f26.f
	$(F77) $(SP_FFLAGS) -c f26.f
	rm -f f26.f
	
f27.o	work/f27.r:
	$(RF) $(SP_RFLAGS) work/f27.r >f27.f
	$(F77) $(SP_FFLAGS) -c f27.f
	rm -f f27.f
	
f28.o	work/f28.r:
	$(RF) $(SP_RFLAGS) work/f28.r >f28.f
	$(F77) $(SP_FFLAGS) -c f28.f
	rm -f f28.f
	
f29.o	work/f29.r:
	$(RF) $(SP_RFLAGS) work/f29.r >f29.f
	$(F77) $(SP_FFLAGS) -c f29.f
	rm -f f29.f

f39.o	work/f39.r:
	$(RF) $(SP_RFLAGS) work/f39.r >f39.f
	$(F77) $(SP_FFLAGS) -c f39.f
	rm -f f39.f
