# the standard common block for wavelength header plus complete data set:

        common/lblwav/  iwcflg, iwtitl, uwsrnm, iwscta, iwsctb, jdwtea
        common/lblwav/  jdwteb, iwstb, iwsra, iwsdec, iwtchn, iwrmas
        common/lblwav/  wrevs, iwband(2), iwrwav, iwresp, iwrecn
	common/lblwav/  iwtpnt, iwhist, mwhist
	common/lblwav/  nwruns, wiangl, weangl, wphase, wwtrns, wtimch
        common/lblwav/  xwnrm, wcatim, twmint, wtempd
        common/lblwav/  wdata(4864)

        character*40    iwtitl
        character*60    iwhist
        character*296   mwhist
	character*8	uwsrnm

	integer*4 iwcflg, iwscta, iwsctb, jdwtea
	integer*4 jdwteb, iwstb, iwsra, iwsdec, iwtchn, iwrmas
	integer*4 wrevs, iwband, iwrwav, iwresp, iwrecn, iwtpnt
	integer*4 nwruns, wiangl, weangl, wphase, wwtrns, wtimch

	real*4 xwnrm, wcatim, twmint, wtempd
	real*4 wdata

# the following is for io buffering to disk

	integer*4 iowbuf
	common /wavio/ iowbuf(384)
