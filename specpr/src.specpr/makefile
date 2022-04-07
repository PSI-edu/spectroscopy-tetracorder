#
# SPECPR main makefile
#
SHELL=/bin/sh

# to add a new directory, add to DIRS and OBJ.  That's all!

DIRS= \
common 3dio add band crtp dtol dummy dvml extn fass \
fcn01-08 fcn09-11 fcn12-13 fcn14-16 fcn17-19 \
fcn20-25 fcn35-39 fcn40-42 fcn43-47 fcn48-50 \
free gould hed hpgraph init interbandanl io lib main \
mth1 mth2 newplot parser rst sub syslib $(SPSDIR) talg \
wav mingw-regex xwindow

OBJ_INIT= \
$(SP_OBJ)/3dio.o $(SP_OBJ)/add.o $(SP_OBJ)/band.o $(SP_OBJ)/crtp.o \
$(SP_OBJ)/dtol.o $(SP_OBJ)/dummy.o $(SP_OBJ)/dvml.o $(SP_OBJ)/extn.o \
$(SP_OBJ)/fass.o $(SP_OBJ)/fcn01-08.o $(SP_OBJ)/fcn09-11.o \
$(SP_OBJ)/fcn12-13.o $(SP_OBJ)/fcn14-16.o $(SP_OBJ)/fcn17-19.o \
$(SP_OBJ)/fcn20-25.o $(SP_OBJ)/fcn35-39.o $(SP_OBJ)/fcn40-42.o \
$(SP_OBJ)/fcn43-47.o $(SP_OBJ)/fcn48-50.o $(SP_OBJ)/free.o \
$(SP_OBJ)/gould.o $(SP_OBJ)/hed.o $(SP_OBJ)/hpgraph.o $(SP_OBJ)/init.o \
$(SP_OBJ)/interbandanl.o $(SP_OBJ)/io.o $(SP_OBJ)/lib.o $(SP_OBJ)/main.o \
$(SP_OBJ)/mth1.o $(SP_OBJ)/mth2.o $(SP_OBJ)/newplot.o \
$(SP_OBJ)/parser.o $(SP_OBJ)/rst.o $(SP_OBJ)/sub.o \
$(SP_OBJ)/syslib.o $(SPSYSOBJ) $(SP_OBJ)/talg.o \
$(SP_OBJ)/wav.o

ifdef WIN
     OBJ= $(OBJ_INIT) $(SP_OBJ)/mingw-regex.o
else
     OBJ= $(OBJ_INIT) $(SP_OBJ)/xwindow.o
endif

# make all does a compile from scratch: it goes into all
#      subdirectories and does a make on each.
#
all:	$(SP_OBJ)/common.o all_dirs $(SP_LIB)/specpr.a
	check_env SP_OBJ SP_LIB F77 SP_FFLAGS SP_LDFLAGS SP_LIB SP_LDLIBS
	$(F77) $(SP_FFLAGS) -v -O $(SP_LDFLAGS) -o xdspecpr main/specpr.o \
		$(SP_OBJ)/common.o \
		$(SP_LIB)/specpr.a $(SP_LDLIBS)
	if test -f dspecpr ; then mv dspecpr dspecpr.old1 ; fi
	mv xdspecpr dspecpr
#	rm dspecpr.old1


$(SP_OBJ)/common.o:
	check_env SP_OBJ SP_LIB F77 SP_FFLAGS SP_LDFLAGS SP_LIB SP_LDLIBS
	@(cd common ; echo "making in common" ; make)

all_dirs:	$(SP_OBJ)/common.o $(OBJ)

#
# this one checks all $(SP_OBJ)ects
#
specpr.a:	$(SP_OBJ)/common.o $(OBJ)
	check_env SP_OBJ SP_LIB AR SP_ARFLAGS RANLIB
	rm -f $(SP_LIB)/specpr.a
	$(AR) $(SP_ARFLAGS) $(SP_LIB)/specpr.a $(SP_OBJ)/common.o $(OBJ)
	$(RANLIB) $(SP_LIB)/specpr.a

$(SP_LIB)/specpr.a:	$(SP_OBJ)/common.o $(OBJ)
	check_env SP_OBJ SP_LIB AR SP_ARFLAGS RANLIB
	rm -f $(SP_LIB)/specpr.a
	$(AR) $(SP_ARFLAGS) $(SP_LIB)/specpr.a $(SP_OBJ)/common.o $(OBJ)
	$(RANLIB) $(SP_LIB)/specpr.a

$(DIRS)::
	@(cd $@ ; echo "making all in $@" ; make)
	@echo "*** specpr.a may need to be remade.. If so, type 'make specpr.a'"

$(OBJ)::	
	@(cd $(@F:.o=) ; echo "making all in $(@F:.o=)" ; make)

# removes big .o files, temp and debug and specpr.a
#         it does not remove .o in each sub directory.
clean:
	check_env SP_OBJ SP_LIB
	-rm -f $(SP_OBJ)/* ../tmp/* ../debug/* $(SP_LIB)/specpr.a

# this removes what clean does plus all .o's in sub dirs

really_clean:
	check_env SP_OBJ SP_LIB
	-rm -f $(SP_OBJ)/* ../tmp/* ../debug/* $(SP_LIB)/specpr.a
	for i in $(DIRS) ;\
	do \
		(cd $$i ; echo "cleaning all in $$i" ; rm -f *.o) ;\
	done

temp:	
	check_env SP_OBJ SP_LIB F77 SP_FFLAGS SP_LDFLAGS SP_LIB SP_LDLIBS
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o xdspecpr main/specpr.o $(SP_OBJ)/common.o \
	$(OBJ) $(SP_LDLIBS)
	mv xdspecpr dspecpr

dspecpr:
		check_env SP_OBJ SP_LIB F77 SP_FFLAGS SP_LDFLAGS SP_LIB SP_LDLIBS
		$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o xdspecpr main/specpr.o \
		$(SP_OBJ)/common.o \
		$(SP_LIB)/specpr.a $(SP_LDLIBS)
		mv xdspecpr dspecpr

install:	dspecpr
	check_env SP_LOCAL
	cp dspecpr $(SP_LOCAL)/bin/pspecpr

	# done elsewhere (2018-10-07)
	# chmod 755 $(SP_LOCAL)/bin/pspecpr
	# cp -p dspecpr.sh $(SP_LOCAL)/bin/dspecpr
	# chmod 755 $(SP_LOCAL)/bin/dspecpr
	# cp -p specpr.sh $(SP_LOCAL)/bin/specpr
	# chmod 755 $(SP_LOCAL)/bin/specpr
