SHELL=/bin/sh
SRCFIL=../../src.specpr

OBJ=		\
b_option.o	\
binary.o	\
getopt.o	\
getcmdargs.o    \
spfeatures.o	\
spopen.o

OBJA= \
$(SRCFIL)/free/wjfren.o	\
$(SRCFIL)/free/decod1.o	\
$(SRCFIL)/free/ihchar.o	\
$(SRCFIL)/gould/lnb.o \
$(SRCFIL)/gould/fnb.o \
$(SRCFIL)/syslib/fsize.o \
$(SRCFIL)/hpgraph/tabbin.o


spfeatures:	$(OBJ)
	$(F77) $(FFLAGS) -o spfeatures $(OBJ) $(OBJA)

.SUFFIXES:
.SUFFIXES: .o .r

spfeatures.o:		spfeatures.r
	$(SSPP) -r -$(SSPPFLAGS) spfeatures.r > spfeatures.x
	$(RF) $(RFLAGS) spfeatures.x > spfeatures.f
	$(F77) $(FFLAGS) -c spfeatures.f
	rm -f spfeatures.x spfeatures.f

b_option.o:		b_option.r
	$(RF) $(RFLAGS) b_option.r > b_option.f
	$(F77) $(FFLAGS) -c b_option.f
	rm -f b_option.f

binary.o:		binary.r
	$(RF) $(RFLAGS) binary.r > binary.f
	$(F77) $(FFLAGS) -c binary.f
	rm -f binary.f

getopt.o:		getopt.r
	$(RF) $(RFLAGS) getopt.r > getopt.f
	$(F77) $(FFLAGS) -c getopt.f
	rm -f getopt.f

getcmdargs.o:		getcmdargs.r
	$(SSPP) -r -$(SSPPFLAGS) getcmdargs.r > getcmdargs.x
	$(RF) $(RFLAGS) getcmdargs.x > getcmdargs.f
	$(F77) $(FFLAGS) -c getcmdargs.f
	rm -f getcmdargs.x getcmdargs.f

spopen.o:		spopen.r
	$(RF) $(RFLAGS) spopen.r > spopen.f
	$(F77) $(FFLAGS) -c spopen.f
	rm -f spopen.f

install:
	mv spfeatures $(SP_LOCAL)/bin/spfeatures
	chmod 755 $(SP_LOCAL)/bin/spfeatures

