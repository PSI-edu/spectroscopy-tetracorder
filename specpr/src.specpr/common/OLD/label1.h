#     /label1/ common:
#       the data making up the standard specpr file format .
#       cta   : civil or universal time when the data was written.
#       ctb   : civil or universal time when the data run was started.
#       sta   : siderial time when the data was written.
#       stb   : siderial time when the data run was started.
#       datea : date data run was written.
#       dateb : data data run was started.
#       revs  : number of revolutions or scans of the spectrum in a scanning
#          spectrometer (eg: a cvf).
#       filno : file number ( technically a record number ).
#       ira(3): right ascension ( hours, min., secs.)
#       idec(3) : dectination ( deg., min., sec. )
#       irmas : air mass times 1000 (integer).
#       ifut(1) : band normalization lower channel limit.
#       ifut(2) :   "        "       upper    "      "
#       ifut (3 to 7) unused integer variables.
#       itimch  : time observed per object per half chop in milliseconds.
#       ihist   : program automatic history.
#       mhist   : 296 character manual history (sometimes used automatically
#          by program ).
#       nruns   : number of runs (1 run = 1 spectrum ).  more than 1 run can
#          be averaged or summed.
#       ieros   : 1 sigma error bars are located in next record if =1
#          ( variable not fully implemented yet ).
#       iwtrns  : weighted number of runs (not fully implemented yet).
#       xnrm    : band normalization ( scaling factor ) =1.0 for raw data.
#       scatim  : time in seconds to scan spectrum.
#       timint  : total integrating time
#       xfut    : unused variables
#       data    : data
#
# the standard common block for header plus complete data set:

        common/label1/  icflag, ititl, usernm, iscta, isctb, jdatea
        common/label1/  jdateb, istb, isra, isdec, itchan, irmas
        common/label1/  revs, iband(2), irwav, irespt, irecno
	common/label1/  itpntr, ihist, mhist
	common/label1/  nruns, siangl, seangl, sphase, iwtrns, itimch
        common/label1/  xnrm, scatim, timint, tempd
        common/label1/  data(4864)

        character*40    ititl
        character*60    ihist
        character*296   mhist
	character*8	usernm

	integer*4 icflag, iscta, isctb, jdatea
	integer*4 jdateb, istb, isra, isdec, itchan, irmas
	integer*4 revs, iband, irwav, irespt, irecno, itpntr
	integer*4 nruns, siangl, seangl, sphase, iwtrns, itimch

	real*4 xnrm, scatim, timint, tempd, xfut
	real*4 data

# the following is for compatability with some existing code

        integer*4 ifut(4), filno
	equivalence (iband(1), ifut(1))
	equivalence (irecno, filno)

# the following is for the case when the header is all text

	integer*4 itxtpt
	integer*4 itxtch
	character*19680 itext
	equivalence (iscta,itxtpt), (jdatea,itext(1:1))
	equivalence (isctb,itxtch)

#
# the following included for compatability with the old format
#
	common/labl1b/ cta(3),ctb(3),sta(3),stb(3),datea(3),dateb(3)
	common/labl1b/ ira(3), idec(3)
        character*2     cta,ctb,sta,stb,datea,dateb
	integer*2 	ira, idec

# question the need for sta (4/3/85)

# the I/O common block:

	common /recio/ iobuff(384)
	integer*4 iobuff


# now include lmaxes here, because the label1 common is the default
#     data array sizes, and file i/o

	common/lmaxes/ maxrec, maxchn, maxtxt
	integer*4 maxrec, maxchn, maxtxt
