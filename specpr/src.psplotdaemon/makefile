#
#	%W%	%G%	%U%
#	makefile for hp pen plotter spooler filter
#	J.A.Hoover	initial version 5/16/84
#       modified for hpgldaemon R. Clark 1/10/95
#
#       03/13/2006 Randall Dailey
#		Using makefile for hpgldaemon, modified for psplotdaemon
#       03/21/2006 Randall Dailey
#		greek.c, greek.o, pagefeed.c, pagefeed.o, initport.c, and
#		initport.o removed from makefile since not needed.  OBJ2 and
#		initpltr2 references removed since not needed.
#		The modified source remains in case needed in the future.
SRC=	\
finishplt.c	\
finishup.c	\
psplotdaemon.c	\
psinitpltr.c	\
logerr.c	\
openargs.c	\
plotem.c

OBJ=	\
finishplt.o	\
finishup.o	\
psplotdaemon.o	\
psinitpltr.o	\
logerr.o	\
openargs.o	\
plotem.o

HDR=psdaemon.h 

CUTTER=CUTTER
CMODE=RS232
LPR=vcat
SHELL=/bin/sh

psplotdaemon:	$(HDR) $(OBJ)
	cc $(CFLAGS) -o psplotdaemon $(OBJ) -lm $(LSTRING) 

psinitpltr.o:	psinitpltr.c
	cc $(CFLAGS) -DPLOTTER -D$(CMODE) -c -o psinitpltr.o psinitpltr.c

finishplt.o:	finishplt.c
	cc  $(CFLAGS) -D$(CMODE) -c -o finishplt.o finishplt.c

finishup.o:	finishup.c
	cc $(CFLAGS) -D$(CMODE) -c -o finishup.o finishup.c

psplotdaemon.o:	psplotdaemon.c
	cc $(CFLAGS) -D$(CMODE) -c -o psplotdaemon.o psplotdaemon.c

logerr.o:	logerr.c
	cc $(CFLAGS) -D$(CMODE) -c -o logerr.o logerr.c

openargs.o:	openargs.c
	cc $(CFLAGS) -D$(CMODE) -c -o openargs.o openargs.c

plotem.o:	plotem.cc
	$(SSPP) -c -$(SSPPFLAGS) plotem.cc > plotem.c
	cc $(CFLAGS) -D$(CMODE) -c -o plotem.o plotem.c
	rm -f plotem.c


install:
	mv -f psplotdaemon $(SP_LOCAL)/bin/psplotdaemon
	chmod 755 $(SP_LOCAL)/bin/psplotdaemon

finishplt.o:	psdaemon.h
finishup.o:	psdaemon.h
psplothpdaemon.o:	psdaemon.h
psinitpltr.o:	psdaemon.h
initpltr2.o:	psdaemon.h
initport.o:	psdaemon.h
initport2.o:	psdaemon.h
logerr.o:	psdaemon.h
openargs.o:	psdaemon.h
pagefeed.o:	psdaemon.h
plotem.o:	psdaemon.h
