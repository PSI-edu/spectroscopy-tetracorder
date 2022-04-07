# /etc/csh.cshrc: system-wide .cshrc file for csh(1) and tcsh(1)
# 

setenv LD_LIBRARY_PATH '/usr/local/lib /usr/lib/x86_64-linux-gnu'

setenv GFORTRAN_UNBUFFERED_ALL  y
setenv GFORTRAN_ERROR_BACKTRACE y
setenv GFORTRAN_ERROR_DUMPCORE  n

# specpr config:

# note: in SSPPFLAGS, set TERMDELAY if you need extra delays by the
# cpu for graphics terminal graphics.
setenv SSPPFLAGS 'LINUX -INTEL  -XWIN '
setenv SPECPR /src/local/specpr
setenv RANDRET 32767
setenv SP_LOCAL /usr/local
setenv SP_BIN securebin

# the following is needed for Linux red Hat 8, 9, ....
# but not before. So comment out the one not needed
#
### does not work setenv LDLIBS '/usr/lib/libc.a -L/usr/X11R6/lib -lX11'
# setenv SP_LDFLAGS -L/usr/lib64
# setenv SP_LDLIBS '-L/usr/X11R6/lib -lX11 -lgfortran'
# setenv SP_LDLIBS '-L/usr/X11R6/lib -lX11'
setenv SP_LDFLAGS ' '
setenv SP_LDLIBS '-lX11'

#
# The following should be fine as is, except FFLAGS might need
#               floating point accelerator flags
#               (e.g. on sun add -ffpa if you have an accelerator)
#
setenv SPSDIR syslinux
setenv SPSYSOBJ '${SP_OBJ}/syslinux.o'
setenv RANLIB 'ranlib'
setenv SSPP 'sspp'
setenv SP_DBG '${SPECPR}/debug'
setenv SP_TMP '${SPECPR}/tmp'
setenv SP_OBJ '${SPECPR}/obj'
setenv SP_LIB '${SPECPR}/lib'

setenv F77 gfortran
setenv CC 'cc'
setenv AR 'ar'
setenv RF 'ratfor'
setenv YACC yacc
setenv LEX  flex

setenv SP_FFLAGS '-C -O -m64'
setenv SP_FFLAGS1 '-C -m64'
setenv SP_FFLAGS2 '-C -m64'
setenv SPKLUDGE LINUX

# Added to following to not interrupt a backslash
setenv BSLASH '-fno-backslash'

setenv SP_FOPT '-O'
setenv SP_FOPT1 '-O'
setenv SP_FOPT2 '-O'
setenv SP_RFLAGS '<'
setenv SP_CFLAGS '-O -m64'
setenv SP_ARFLAGS 'rv'
setenv SP_GFLAGS '-s'
setenv SP_LFLAGS ' '
setenv SP_YFLAGS ' '

# for davinci:
setenv LD_RUN_PATH '/usr/local/lib'

