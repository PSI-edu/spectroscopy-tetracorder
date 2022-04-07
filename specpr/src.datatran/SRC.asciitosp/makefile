SHELL=/bin/sh

OBJ=		\
asciitosp.o


asciitosp:	$(OBJ)
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o asciitosp $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


asciitosp.o:		asciitosp.r
	$(SSPP) -r -$(SSPPFLAGS)  asciitosp.r > asciitosp.x
	$(RF) $(SP_RFLAGS) asciitosp.x > asciitosp.f
	$(F77) $(SP_FFLAGS) -c asciitosp.f
	rm -f asciitosp.x asciitosp.f

install:
	mv -f asciitosp $(SP_LOCAL)/bin/asciitosp
	chmod 755 $(SP_LOCAL)/bin/asciitosp
