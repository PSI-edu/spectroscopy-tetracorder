# @(#) makefile 2.1@(#)

SHELL=/bin/sh
GET=sp_if_get

OBJ=	\
recomp.o \
units.o  \
re_exec.o

$(SP_OBJ)/syslinux.o:	$(OBJ)
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

re_exec.o:	re_exec.c
	$(CC) $(SP_CFLAGS) -c re_exec.c

recomp.o:	recomp.f
	$(F77) $(SP_FFLAGS) -c recomp.f

units.o:	units.c
	$(CC) $(SP_CFLAGS) -c units.c
