# @(#) makefile 2.2@(#)

SHELL=/bin/sh

OBJ= \
conanl.o \
dpfix.o \
f44.o \
fill.o \
hull.o \
lowp.o \
confix.o \
pfind.o	\
remove.o \
f43.o \
fft.o \
f46.o \
f47.o \
f45.o \
featur.o \
zstuf.o \
zfirst.o \
zfeat.o \
should.o \
spopen.o 

#fft.o


$(SP_OBJ)/fcn43-47.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)


fft.o:		fft.r
	$(RF) $(SP_RFLAGS) fft.r > fft.f
	$(F77) $(SP_FFLAGS) -c fft.f
	rm -f fft.f

f43.o:		f43.r
	$(RF) $(SP_RFLAGS) f43.r > f43.f
	$(F77) $(SP_FFLAGS) -c f43.f
	rm -f f43.f

conanl.o:		conanl.r
	$(RF) $(SP_RFLAGS) conanl.r > conanl.f
	$(F77) $(SP_FFLAGS) -c conanl.f
	rm -f conanl.f

dpfix.o:		dpfix.r
	$(RF) $(SP_RFLAGS) dpfix.r > dpfix.f
	$(F77) $(SP_FFLAGS) -c dpfix.f
	rm -f dpfix.f

f44.o:		f44.r
	$(RF) $(SP_RFLAGS) f44.r > f44.f
	$(F77) $(SP_FFLAGS) -c f44.f
	rm -f f44.f

fill.o:		fill.r
	$(RF) $(SP_RFLAGS) fill.r > fill.f
	$(F77) $(SP_FFLAGS) -c fill.f
	rm -f fill.f

hull.o:		hull.r
	$(RF) $(SP_RFLAGS) hull.r > hull.f
	$(F77) $(SP_FFLAGS) -c hull.f
	rm -f hull.f

lowp.o:		lowp.r
	$(RF) $(SP_RFLAGS) lowp.r > lowp.f
	$(F77) $(SP_FFLAGS) -c lowp.f
	rm -f lowp.f

pfind.o:		pfind.r
	$(RF) $(SP_RFLAGS) pfind.r > pfind.f
	$(F77) $(SP_FFLAGS) -c pfind.f
	rm -f pfind.f

remove.o:		remove.r
	$(RF) $(SP_RFLAGS) remove.r > remove.f
	$(F77) $(SP_FFLAGS) -c remove.f
	rm -f remove.f

confix.o:		confix.r
	$(RF) $(SP_RFLAGS) confix.r > confix.f
	$(F77) $(SP_FFLAGS) -c confix.f
	rm -f confix.f

f45.o:		f45.r
	$(RF) $(SP_RFLAGS) f45.r > f45.f
	$(F77) $(SP_FFLAGS) -c f45.f
	rm -f f45.f

f46.o:		f46.r
	$(RF) $(SP_RFLAGS) f46.r > f46.f
	$(F77) $(SP_FFLAGS) -c f46.f
	rm -f f46.f

f47.o:		f47.r
	$(RF) $(SP_RFLAGS) f47.r > f47.f
	$(F77) $(SP_FFLAGS) -c f47.f
	rm -f f47.f

featur.o:		featur.r
	$(RF) $(SP_RFLAGS) featur.r > featur.f
	$(F77) $(SP_FFLAGS) -c featur.f
	rm -f featur.f

zstuf.o:		zstuf.r
	$(RF) $(SP_RFLAGS) zstuf.r > zstuf.f
	$(F77) $(SP_FFLAGS) -c zstuf.f
	rm -f zstuf.f

zfirst.o:		zfirst.r
	$(RF) $(SP_RFLAGS) zfirst.r > zfirst.f
	$(F77) $(SP_FFLAGS) -c zfirst.f
	rm -f zfirst.f

zfeat.o:		zfeat.r
	$(RF) $(SP_RFLAGS) zfeat.r > zfeat.f
	$(F77) $(SP_FFLAGS) -c zfeat.f
	rm -f zfeat.f

should.o:		should.r
	$(RF) $(SP_RFLAGS) should.r > should.f
	$(F77) $(SP_FFLAGS) -c should.f
	rm -f should.f

spopen.o:		spopen.r
	$(RF) $(SP_RFLAGS) spopen.r > spopen.f
	$(F77) $(SP_FFLAGS) -c spopen.f
	rm -f spopen.f
