#CFLAGS=-N -Aa -D_HPUX_SOURCE

sptgifdaemon:
	cc $(CFLAGS) -o sptgifdaemon sptgifdaemon.c

install:
	mv -f sptgifdaemon $(SP_LOCAL)/bin/sptgifdaemon
	chmod 755 $(SP_LOCAL)/bin/sptgifdaemon
