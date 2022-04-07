	subroutine chplta (nchans,xmax,xmin,x,iline)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine draws the tick marks for the
#ccc                   horizontal axis of the crt plots when plotting
#ccc                   is done in channel.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    movabs,drwabs,sb
#ccc  argument list description:
#ccc    nchans  input   number of channels
#ccc    xmax    output  maximum x-axis value plotted
#ccc    xmin    output  minimum x-axis value plotted
#ccc    iline   ??????  line type (unused by this routine)
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/dscrch"

	character*80 outline

	real*4 x(SPMAXCHAN)

	xmin = 0.
	if (nchans < 0) nchans = 1
	xmax = nchans
#
#     set x values tequal to channel number
#
	do i = 1,nchans
		x(i) = i

#
#     if channels less than ro equal to 30 then
#     don't connect points: iline = 0
#
#
#     determine tick spacing on x-axis
#
	strt = 0.0
	fin = nchans
	call inice(strt,fin,bint,12.,astrt)
#
#     draw tick marks and label x-axis
#
#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.

	iayl = ayl
	intx = bint
	do i= 1,12 {
		ichnum = (i-1)*intx
		if (ichnum == 0) ichnum=1
		if (ichnum > nchans) ichnum=nchans
		xx= (float(ichnum)/float(nchans))* (axh-axl) + axl + 0.5
		ix= xx
		call movabs (ix, iayl)
		call drwabs (ix, iayl-3)
		call movabs (ix-19, iayl-18)
		call sb(0)
		write (outline, 115) ichnum,char(0)
		call gwrite(outline)
		if (ichnum >= nchans) break
	}
	call movabs (200, 17)
	call sb (0)
	write (outline, 250) char(0)
	call gwrite(outline)
115	format (i4,a1)
250	format ('channel ',a1)
	return
	end
