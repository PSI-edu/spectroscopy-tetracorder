# @(#) makefile 1.6@(#)

SHELL=/bin/sh

OBJ= \
read3d.o \
file3d.o \
hedr3d.o \
blkext.o \
redbil.o \
redbsq.o \
redbip.o \
lstrec.o \
num2ch.o \
seth3d.o
#filevic.o 
#hedrvic.o


$(SP_OBJ)/3dio.o:	$(OBJ) ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $(SP_OBJ)/3dio.o $(OBJ)


read3d.o:		read3d.r
	$(RF) $(SP_RFLAGS) read3d.r > read3d.f
	$(F77) $(SP_FFLAGS) -c read3d.f
	rm -f read3d.f

file3d.o:		file3d.r
	$(RF) $(SP_RFLAGS) file3d.r > file3d.f
	$(F77) $(SP_FFLAGS) -c file3d.f
	rm -f file3d.f

hedr3d.o:		hedr3d.r
	$(RF) $(SP_RFLAGS) hedr3d.r > hedr3d.f
	$(F77) $(SP_FFLAGS) -c hedr3d.f
	rm -f hedr3d.f

filevic.o:		filevic.r
	$(RF) $(SP_RFLAGS) filevic.r > filevic.f
	$(F77) $(SP_FFLAGS) -c filevic.f
	rm -f filevic.f

hedrvic.o:		.hedrvic.r
	$(RF) $(SP_RFLAGS) hedrvic.r > hedrvic.f
	$(F77) $(SP_FFLAGS) -c hedrvic.f
	rm -f hedrvic.f

blkext.o:		blkext.r
	$(RF) $(SP_RFLAGS) blkext.r > blkext.f
	$(F77) $(SP_FFLAGS) -c blkext.f
	rm -f blkext.f

redbsq.o:		redbsq.r
	$(RF) $(SP_RFLAGS) redbsq.r > redbsq.f
	$(F77) $(SP_FFLAGS) -c redbsq.f
	rm -f redbsq.f

redbil.o:		redbil.r
	$(RF) $(SP_RFLAGS) redbil.r > redbil.f
	$(F77) $(SP_FFLAGS) -c redbil.f
	rm -f redbil.f

redbip.o:		redbip.r
	$(RF) $(SP_RFLAGS) redbip.r > redbip.f
	$(F77) $(SP_FFLAGS) -c redbip.f
	rm -f redbip.f

seth3d.o:		seth3d.r
	$(RF) $(SP_RFLAGS) seth3d.r > seth3d.f
	$(F77) $(SP_FFLAGS) -c seth3d.f
	rm -f seth3d.f

lstrec.o:		lstrec.r
	$(RF) $(SP_RFLAGS) lstrec.r > lstrec.f
	$(F77) $(SP_FFLAGS) -c lstrec.f
	rm -f lstrec.f

num2ch.o:		num2ch.r
	$(RF) $(SP_RFLAGS) num2ch.r > num2ch.f
	$(F77) $(SP_FFLAGS) -c num2ch.f
	rm -f num2ch.f

blkext.o: ../common/dscrch

blkext.o: ../common/lbl3

file3d.o hedr3d.o read3d.o redbil.o redbip.o redbsq.o seth3d.o lstrec.o: ../common/label1

blkext.o file3d.o hedr3d.o read3d.o redbil.o redbip.o redbsq.o seth3d.o lstrec.o: ../common/ioftyp

blkext.o file3d.o hedr3d.o read3d.o redbil.o redbip.o redbsq.o seth3d.o lstrec.o: ../common/lundefs

file3d.o: ../common/alphabet

hedr3d.o: ../common/lbl4

read3d.o: ../common/label3 ../common/label1
