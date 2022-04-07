# %A%

SHELL=/bin/sh
GET=sp_if_get

OBJ=	\
finishplt.o \
finishup.o \
initplot.o	\
logerr.o 	\
main.o \
openargs.o 	\
plotem.o 	\
stuff.o

vspecplot:	$(OBJ)   ../common/spblockdata.o
	cc $(SP_CFLAGS) -o $@ $(OBJ)

finishplt.o: 	finishplt.c
	cc $(SP_CFLAGS) -c finishplt.c

finishup.o: 	finishup.c
	cc $(SP_CFLAGS) -c finishup.c

initplot.o:	initplot.c
	cc $(SP_CFLAGS) -c initplot.c

logerr.o: 	logerr.c
	cc $(SP_CFLAGS) -c logerr.c

main.o: 	main.c
	cc $(SP_CFLAGS) -c main.c

openargs.o: 	openargs.c
	cc $(SP_CFLAGS) -c openargs.c

plotem.o: 	plotem.c
	cc $(SP_CFLAGS) -c plotem.c

stuff.o:	stuff.c
	cc $(SP_CFLAGS) -c stuff.c

logerr.o:	splot.h
main.o:		splot.h
openargs.o:	splot.h
plotem.o:	splot.h
finishplt.o:	splot.h
finishup.o:	splot.h
initplot.o:	splot.h
stuff.o:

