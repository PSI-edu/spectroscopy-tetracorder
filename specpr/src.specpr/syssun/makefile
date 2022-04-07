# @(#) makefile 2.1@(#)

SHELL=/bin/sh
GET=sp_if_get

OBJ=	\
units.o

$(SP_OBJ)/syssun.o:	$(OBJ)
	ld $(SP_LFLAGS) -r -o $@ $(OBJ)

units.o:	units.c
	$(CC) $(SP_CFLAGS) -c units.c
