# map of data type for the first record.  The map has
# iomap(i) = 0  = ascii
# iomap(i) = 1  = binary.
# note: iomap(1) = always as this is the bit flags

        integer*4 iomap(384)

	data iomap /1,
		0,0,0,0,0,0,0,0,0,0,
		0,0,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1 /


#      common/label1/  icflag, ititl, usernm, iscta, isctb, jdatea
#      common/label1/  jdateb, istb, isra, isdec, itchan, irmas
#      common/label1/  revs, iband(2), irwav, irespt, irecno
#      common/label1/  itpntr, ihist, mhist
#      common/label1/  nruns, siangl, seangl, sphase, iwtrns, itimch
#      common/label1/  xnrm, scatim, timint, tempd
#      common/label1/  data(256)
#
#      character*40    ititl
#      character*60    ihist
#      character*296   mhist
#      character*8     usernm
#
#      integer*4 icflag, iscta, isctb, jdatea
#      integer*4 jdateb, istb, isra, isdec, itchan, irmas
#      integer*4 revs, iband, irwav, irespt, irecno, itpntr
#      integer*4 nruns, siangl, seangl, sphase, iwtrns, itimch
#
#      real*4 xnrm, scatim, timint, tempd, xfut
#      real*4 data

