
SHELL=/bin/sh

OBJ= \
absbin.o \
convrt.o \
drwabs.o \
eralph.o \
gwrite.o \
hbell.o \
hpabs.o \
hpline.o \
hpplot.o \
hreset.o \
initt.o \
movabs.o \
newpag.o \
othtrm.o \
respag.o \
sb.o \
serase.o \
tabbin.o \
texmod.o \
tmove.o \
vgrmod.o 


$(SP_OBJ)/hpgraph.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r .c
	
absbin.o:		absbin.r
	$(RF) $(SP_RFLAGS) absbin.r > absbin.f
	$(F77) $(SP_FFLAGS) -c absbin.f
	rm -f absbin.f

convrt.o:		convrt.r
	$(RF) $(SP_RFLAGS) convrt.r > convrt.f
	$(F77) $(SP_FFLAGS) -c convrt.f
	rm -f convrt.f

drwabs.o:		drwabs.r
	$(SSPP) -r -$(SSPPFLAGS)  drwabs.r > drwabs.x
	$(RF) $(SP_RFLAGS) drwabs.x > drwabs.f
	$(F77) $(SP_FFLAGS) -c drwabs.f
	rm -f drwabs.x drwabs.f

eralph.o:		eralph.r
	$(RF) $(SP_RFLAGS) eralph.r > eralph.f
	$(F77) $(SP_FFLAGS) -c eralph.f
	rm -f eralph.f

hbell.o:		hbell.r
	$(RF) $(SP_RFLAGS) hbell.r > hbell.f
	$(F77) $(SP_FFLAGS) -c hbell.f
	rm -f hbell.f

hpabs.o:		hpabs.r
	$(SSPP) -r -$(SSPPFLAGS) hpabs.r > hpabs.x
	$(RF) $(SP_RFLAGS) hpabs.x > hpabs.f
	$(F77) $(SP_FFLAGS) -c hpabs.f
	rm -f hpabs.x hpabs.f

hpline.o:		hpline.r
	$(SSPP) -r -$(SSPPFLAGS)  hpline.r >hpline.x
	$(RF) $(SP_RFLAGS) hpline.x > hpline.f
	$(F77) $(SP_FFLAGS) -c hpline.f
	rm -f hpline.x hpline.f

hpplot.o:		hpplot.r
	$(RF) $(SP_RFLAGS) hpplot.r > hpplot.f
	$(F77) $(SP_FFLAGS) -c hpplot.f
	rm -f hpplot.f

hreset.o:		hreset.r
	$(RF) $(SP_RFLAGS) hreset.r > hreset.f
	$(F77) $(SP_FFLAGS) -c hreset.f
	rm -f hreset.f

initt.o:		initt.r
	$(SSPP) -r -$(SSPPFLAGS)  initt.r >initt.x
	$(RF) $(SP_RFLAGS) initt.x > initt.f
	$(F77) $(SP_FFLAGS) -c initt.f
	rm -f initt.x initt.f

movabs.o:		movabs.r
	$(RF) $(SP_RFLAGS) movabs.r > movabs.f
	$(F77) $(SP_FFLAGS) -c movabs.f
	rm -f movabs.f

newpag.o:		newpag.r
	$(RF) $(SP_RFLAGS) newpag.r > newpag.f
	$(F77) $(SP_FFLAGS) -c newpag.f
	rm -f newpag.f

othtrm.o:		othtrm.r
	$(RF) $(SP_RFLAGS) othtrm.r > othtrm.f
	$(F77) $(SP_FFLAGS) -c othtrm.f
	rm -f othtrm.f

respag.o:		respag.r
	$(SSPP) -r -$(SSPPFLAGS) respag.r >respag.x
	$(RF) $(SP_RFLAGS) respag.x > respag.f
	$(F77) $(SP_FFLAGS) -c respag.f
	rm -f respag.x respag.f

sb.o:		sb.r
	$(SSPP) -r -$(SSPPFLAGS)  sb.r >sb.x
	$(RF) $(SP_RFLAGS) sb.x > sb.f
	$(F77) $(SP_FFLAGS) -c sb.f
	rm -f sb.x sb.f

serase.o:		serase.r
	$(SSPP) -r -$(SSPPFLAGS)  serase.r >serase.x
	$(RF) $(SP_RFLAGS) serase.x > serase.f
	$(F77) $(SP_FFLAGS) -c serase.f
	rm -f serase.x serase.f

tabbin.o:		tabbin.r
	$(RF) $(SP_RFLAGS) tabbin.r > tabbin.f
	$(F77) $(SP_FFLAGS) -c tabbin.f
	rm -f tabbin.f

texmod.o:		texmod.r
	$(RF) $(SP_RFLAGS) texmod.r > texmod.f
	$(F77) $(SP_FFLAGS) -c texmod.f
	rm -f texmod.f

tmove.o:		tmove.r
	$(RF) $(SP_RFLAGS) tmove.r > tmove.f
	$(F77) $(SP_FFLAGS) -c tmove.f
	rm -f tmove.f

vgrmod.o:		vgrmod.r
	$(RF) $(SP_RFLAGS) vgrmod.r > vgrmod.f
	$(F77) $(SP_FFLAGS) -c vgrmod.f
	rm -f vgrmod.f

gwrite.o:		gwrite.r
	$(SSPP) -r -$(SSPPFLAGS)  gwrite.r >gwrite.x
	$(RF) $(SP_RFLAGS) gwrite.x > gwrite.f
	$(F77) $(SP_FFLAGS) -c gwrite.f
	rm -f gwrite.x gwrite.f

hptest:	hptest.r $(OBJ)
	$(RF) $(SP_RFLAGS) hptest.r > hptest.f
	$(F77) $(SP_FFLAGS) hptest.f $(OBJ) ../syslib/fsleep.o \
	../syslib/iwrite.o -o hptest -lX11
	rm -f hptest.f

