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
fft.o \
f43.o \
f46.o \
f47.o \
f45.o \
featur.o \
zstuf.o \
zfirst.o \
zfeat.o \
should.o \
spopen.o 


$(SP_OBJ)/fcn43-47.o:	$(OBJ)
	ld $(LFLAGS) -r -o $@ $(OBJ)


fft.o:		fft.r
	$(RF) $(RFLAGS) fft.r > fft.f
	$(F77) $(FFLAGS) -c fft.f
	rm -f fft.f

f43.o:		f43.r
	$(RF) $(RFLAGS) f43.r > f43.f
	$(F77) $(FFLAGS) -c f43.f
	rm -f f43.f

conanl.o:		conanl.r
	$(RF) $(RFLAGS) conanl.r > conanl.f
	$(F77) $(FFLAGS) -c conanl.f
	rm -f conanl.f

dpfix.o:		dpfix.r
	$(RF) $(RFLAGS) dpfix.r > dpfix.f
	$(F77) $(FFLAGS) -c dpfix.f
	rm -f dpfix.f

f44.o:		f44.r
	$(RF) $(RFLAGS) f44.r > f44.f
	$(F77) $(FFLAGS) -c f44.f
	rm -f f44.f

fill.o:		fill.r
	$(RF) $(RFLAGS) fill.r > fill.f
	$(F77) $(FFLAGS) -c fill.f
	rm -f fill.f

hull.o:		hull.r
	$(RF) $(RFLAGS) hull.r > hull.f
	$(F77) $(FFLAGS) -c hull.f
	rm -f hull.f

lowp.o:		lowp.r
	$(RF) $(RFLAGS) lowp.r > lowp.f
	$(F77) $(FFLAGS) -c lowp.f
	rm -f lowp.f

pfind.o:		pfind.r
	$(RF) $(RFLAGS) pfind.r > pfind.f
	$(F77) $(FFLAGS) -c pfind.f
	rm -f pfind.f

remove.o:		remove.r
	$(RF) $(RFLAGS) remove.r > remove.f
	$(F77) $(FFLAGS) -c remove.f
	rm -f remove.f

confix.o:		confix.r
	$(RF) $(RFLAGS) confix.r > confix.f
	$(F77) $(FFLAGS) -c confix.f
	rm -f confix.f

f45.o:		f45.r
	$(RF) $(RFLAGS) f45.r > f45.f
	$(F77) $(FFLAGS) -c f45.f
	rm -f f45.f

f46.o:		f46.r
	$(RF) $(RFLAGS) f46.r > f46.f
	$(F77) $(FFLAGS) -c f46.f
	rm -f f46.f

f47.o:		f47.r
	$(RF) $(RFLAGS) f47.r > f47.f
	$(F77) $(FFLAGS) -c f47.f
	rm -f f47.f

featur.o:		featur.r
	$(RF) $(RFLAGS) featur.r > featur.f
	$(F77) $(FFLAGS) -c featur.f
	rm -f featur.f

zstuf.o:		zstuf.r
	$(RF) $(RFLAGS) zstuf.r > zstuf.f
	$(F77) $(FFLAGS) -c zstuf.f
	rm -f zstuf.f

zfirst.o:		zfirst.r
	$(RF) $(RFLAGS) zfirst.r > zfirst.f
	$(F77) $(FFLAGS) -c zfirst.f
	rm -f zfirst.f

zfeat.o:		zfeat.r
	$(RF) $(RFLAGS) zfeat.r > zfeat.f
	$(F77) $(FFLAGS) -c zfeat.f
	rm -f zfeat.f

should.o:		should.r
	$(RF) $(RFLAGS) should.r > should.f
	$(F77) $(FFLAGS) -c should.f
	rm -f should.f

spopen.o:		spopen.r
	$(RF) $(RFLAGS) spopen.r > spopen.f
	$(F77) $(FFLAGS) -c spopen.f
	rm -f spopen.f
