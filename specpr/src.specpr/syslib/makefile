# @(#) makefile 2.9@(#)

SHELL=/bin/sh

OBJ=	\
ilink.o 	\
fsleep.o 	\
time.o 	\
iulink.o  \
fsize.o \
iwrite.o	\
iread.o	\
bkrec.o \
frdrec.o	\
rewinf.o	\
wrttap.o	\
redtap.o	\
getpid.o	\
setbit.o	\
clrbit.o	\
chkbit.o	\
chkendian.o	\
fcuser.o	\
fuser.o		\
ixdate.o	\
frandom.o


$(SP_OBJ)/syslib.o:	$(OBJ)	
	check_env SPKLUDGE SP_OBJ
	(cd SRC.$(SPKLUDGE).kludge ; ./rmo ; make) 
	$(LD) $(SP_LFLAGS) -r -o $@ $(OBJ)

.SUFFIXES:
.SUFFIXES: .o .c
	
ilink.o:		ilink.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c ilink.c

fsleep.o:	fsleep.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c fsleep.c

frandom.o:	frandom.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -DRANDRET=$(RANDRET) -c frandom.c

time.o:		time.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c time.c

iulink.o:		iulink.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c iulink.c

fsize.o:		fsize.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c fsize.c

fcuser.o:		fcuser.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c fcuser.c

fuser.o:		fuser.r
	check_env SP_RFLAGS SP_FFLAGS
	$(RF) $(SP_RFLAGS) fuser.r > fuser.f
	$(F77) $(SP_FFLAGS) -c fuser.f
	rm -f fuser.f

iwrite.o:		iwrite.r
	check_env SP_RFLAGS SP_FFLAGS
	$(RF) $(SP_RFLAGS) iwrite.r > iwrite.f
	$(F77) $(SP_FFLAGS) -c iwrite.f
	rm -f iwrite.f

iread.o:		iread.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c iread.c 

bkrec.o:		bkrec.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c bkrec.c

frdrec.o:		frdrec.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c frdrec.c

rewinf.o:		rewinf.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c rewinf.c

wrttap.o:		wrttap.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c wrttap.c

redtap.o:		redtap.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c redtap.c

getpid.o:		getpid.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c getpid.c

setbit.o:		setbit.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c setbit.c

clrbit.o:		clrbit.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c clrbit.c

chkbit.o:		chkbit.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c chkbit.c

chkendian.o:		chkendian.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c chkendian.c

ixdate.o:	ixdate.c
	check_env SP_CFLAGS
	$(CC) $(SP_CFLAGS) -c ixdate.c

