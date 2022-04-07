# @(#) makefile 2.1@(#)

SHELL=/bin/sh

OBJ=	\
nowreading.o \
nowwriting.o \
units.o	\
re_exec.o

$(SP_OBJ)/syshpux.o:	$(OBJ)
	ld $(SP_LFLAGS) -r -o $@ $(OBJ)

iargc.o:	iargc.f
	$(F77) $(SP_FFLAGS) -c iargc.f

getarg.o:	getarg.f
	$(F77) $(SP_FFLAGS) -c getarg.f

nowreading.o:	nowreading.c
	cc $(SP_CFLAGS) -c nowreading.c

nowwriting.o:	nowwriting.c
	cc $(SP_CFLAGS) -c nowwriting.c

units.o:	units.c
	cc $(SP_CFLAGS) -c units.c

re_exec.o:	re_exec.c
	cc $(SP_CFLAGS) -c re_exec.c
