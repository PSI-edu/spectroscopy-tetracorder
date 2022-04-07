	subroutine gpplot
	implicit integer*4(i-n)

	include "../common/spmaxes"   # max parameters, must be first
	
	include "../common/pltcnt"
	include "../common/lundefs"

	integer*4 idummy, irecl, ier, maxchn

	logical getplt

	close(pltlun,iostat=idummy)
######## pre 10/25/2015:
#	open (pltlun,status='scratch',access='direct',form='unformatted',
#		recl=58916)
# note 59136 would be the next higher multiple of 256 (256*231)

        # note recl = 3 times max channels * 4 bytes/chan + 548

	maxchn=SPMAXCHAN

	irecl= maxchn * 12 + 548
	open (pltlun,status='scratch',access='direct',form='unformatted',
		recl=irecl,iostat=ier)
	if (ier != 0) {
		write (ttyout,*) 'gpplot open error:', ier
	}
	pltcnt = 0
	nplots = 0

	while (getplt(ii)) {
		call bldplt
		nplots = 0
	}
	
	close(pltlun,iostat=idummy)
	end
