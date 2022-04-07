SHELL=/bin/sh

SRCFIL=/usr/src/cmd/specpr

OBJ=		\
cgastosp.o


cgastosp:	$(OBJ)
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o cgastosp $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

cgastosp.o:		cgastosp.r
	$(SSPP) -r -$(SSPPFLAGS) cgastosp.r > cgastosp.x
	$(RF) $(SP_RFLAGS) cgastosp.x > cgastosp.f
	$(F77) $(SP_FFLAGS) -c cgastosp.f
	rm -f cgastosp.x cgastosp.f

install:
	mv -f cgastosp $(SP_LOCAL)/bin/cgastosp
	chmod 755 $(SP_LOCAL)/bin/cgastosp
