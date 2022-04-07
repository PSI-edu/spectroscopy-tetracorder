# @(#) makefile 2.2@(#)

SHELL=/bin/sh

OBJ= \
xclear.o \
xcrsrd.o \
xdraw.o \
xerase.o \
xflush.o \
xinit.o \
xread.o \
xtext.o \
xsetline.o \
xwrite.o 


$(SP_OBJ)/xwindow.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r .c
	

xclear.o:	xclear.c
	$(CC) -g -c xclear.c $(SP_CFLAGS)

xcrsrd.o:	xcrsrd.c
	$(CC) -g -c xcrsrd.c $(SP_CFLAGS) 

xdraw.o:	xdraw.c
	$(CC) -g -c  xdraw.c $(SP_CFLAGS)

xerase.o:	xerase.c
	$(CC) -g -c xerase.c $(SP_CFLAGS)

xflush.o:	xflush.c
	$(CC) -g -c xflush.c $(SP_CFLAGS)

xinit.o:	xinit.c
	$(CC) -g -c xinit.c $(SP_CFLAGS)

xread.o:	xread.c
	$(CC) -g -c xread.c $(SP_CFLAGS)

xtext.o:	xtext.c
	$(CC) -g -c xtext.c $(SP_CFLAGS)

xwrite.o:	xwrite.c
	$(CC) -g -c xwrite.c $(SP_CFLAGS)

xsetline.o:	xsetline.c
	$(CC) -g -c  xsetline.c $(SP_CFLAGS)

gtest:	gtest.r $(OBJ)
	$(SSPP) -r -$(SSPPFLAGS) gtest.r > gtest.x
	$(RF) $(SP_RFLAGS) gtest.x > gtest.f
	$(F77) $(SP_FFLAGS) gtest.f ../../lib/specpr.a  -o gtest -lX11
	rm -f gtest.f

