# @(#) makefile 2.4@(#)

SHELL=/bin/sh

OBJ=	\
mthwrt.o	\
opdcod.o	\
redwg2.o	\
gettok.o 	\
itoa.o 		\
type.o 		\
alias.o 

$(SP_OBJ)/mth1.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .r
	
mthwrt.o:		mthwrt.r
	$(RF) $(SP_RFLAGS) mthwrt.r > mthwrt.f
	$(F77) $(SP_FFLAGS) -c mthwrt.f
	rm -f mthwrt.f

opdcod.o:		opdcod.r
	$(RF) $(SP_RFLAGS) opdcod.r > opdcod.f
	$(F77) $(SP_FFLAGS) -c opdcod.f
	rm -f opdcod.f

redwg2.o:		redwg2.r
	$(RF) $(SP_RFLAGS) redwg2.r > redwg2.f
	$(F77) $(SP_FFLAGS) -c redwg2.f
	rm -f redwg2.f

gettok.o:		gettok.r
	$(RF) $(SP_RFLAGS) gettok.r > gettok.f
	$(F77) $(SP_FFLAGS) -c gettok.f
	rm -f gettok.f

itoa.o:		itoa.r
	$(RF) $(SP_RFLAGS) itoa.r > itoa.f
	$(F77) $(SP_FFLAGS) -c itoa.f
	rm -f itoa.f

type.o:		type.r
	$(RF) $(SP_RFLAGS) type.r > type.f
	$(F77) $(SP_FFLAGS) -c type.f
	rm -f type.f

alias.o:		alias.r
	$(RF) $(SP_RFLAGS) alias.r > alias.f
	$(F77) $(SP_FFLAGS) -c alias.f
	rm -f alias.f
