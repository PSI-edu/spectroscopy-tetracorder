subroutine ggauss(wcentr,bwidth,nchans,inwave,gauss)

#ccc  name: ggauss
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description: calculates a Gaussian distribution given
#		center wavelength,input wavelength array and bandwidth
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#
#		wcentr: wavelenght center input
#		bwidth: bandwidth input 
#		nchans: number of channnels in  original input spectrum
#		inwave: the input wavelenght data  
#		gauss : output array containing distribution
#
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

implicit integer*4 (i-n)

include "../common/lundefs"


real*4 wcentr,bwidth
real*4 height,bsqrd
real*4 wave,tmp
real*4 gauss(nchans),inwave(nchans)
integer*4 iwave
integer*4 nchans

#DEBUG:
#######	write (ttyout,*) 'DEBUG Generating a Gaussian: wavelength ',
#######			wcentr,' width ',bwidth
	IDEBUG = 0
#######	if (wcentr > .920 & wcentr < .926) IDEBUG = 1

	height=1.  # defined as one, so removed from computation to
                   # increase speed
	if (bwidth == -1.23e34 || wcentr == -1.23e34) {
		write (ttyout,50)
50		format (' ERROR in Gaussian generation routine:',/,
			' band width or central wavelength deleted')
		return
	}
	if (bwidth <= 1.0e-10)  {
		write (ttyout, 100) bwidth, wcentr
100		format (' WARNING: Gaussian band ',
				'width difference is too small:',/,
			1pe10.3, ' for central wavelength ',1pe10.3,/,
			' RESETTING to 1.0e-10')
		bwidth=1.0e-10
	}
	if (bwidth > 1.0e10)  {
		write (ttyout, 200) bwidth, wcentr
200		format (' WARNING: Gaussian band ',
				'width difference is too large:',/,
			1pe10.3, ' for central wavelength ',1pe10.3,/,
			' RESETTING to 1.0e10')
		bwidth=1.0e-10
	}
#
#       alog(2.0)=0.6931471
#       ax = -1.*alog(2.0)*4.0/bwidth**2
#
	ax=-2.772589/(bwidth**2)	
#
	bxmin = 1e+15  # wavelength diff at wave closest to center of gauss
	ichmin = 0  # closest channel to center of gaussian
	nzeros = 0  # number of gaussian channels set to zero
	ndel = 0  # number of gaussian channels set to zero from deleted points
	ngaus = 0 # number of gaussian points computed

	do iwave=1,nchans  {
		wxx = inwave(iwave)
# DEBUG:
#######		write (ttyout,*) 'DEBUG ggauss ch=',iwave,' wavelength:',wxx

		if (wxx == -1.23e34) {           # check for deleted points
			gauss(iwave) = 0.0
			ndel = ndel + 1
#####			write (ttyout, *) 'DEBUG: width=',bwidth,
#####					'   wave=', wcentr,
#####					'   chan=',iwave,
#####					'   GAUSSIAN VALUE DELETED'
		} else {
			bx = (wxx - wcentr)
			abx = abs(bx)
			if (abx < bxmin) {
				bxmin = abx
				ichmin = iwave
			}
			if (IDEBUG == 1) {
				write(ttyout,*) 'DEBUG ggaus:',
						' ch=',iwave,' wcentr=',
						wcentr,' inp wave=',wxx,
						' diff=',bx,' bwidth=',bwidth
			}
			if (abx > 1.0e16) {  # restrict overflow
				gauss(iwave)=0.0
				nzeros = nzeros + 1
			} else {
				tmp=ax*(bx**2)     # the real computation
				if (tmp < -82.0) { # check for exp overflow
					gauss(iwave)=0.0
					nzeros = nzeros + 1
				} else {
					gauss(iwave)=exp(tmp)
					ngaus = ngaus + 1
				}
			}
			if (IDEBUG == 1) {
				write(ttyout,*) '          gauss=',gauss(iwave)
			}
		}

	}
	if (IDEBUG == 1) write (ttyout,*) 'DEBUG: nzeros=',nzeros,
					'   ndel=',ndel,'    nchans=',nchans
	if (ngaus == 0) {
		write (ttyout,300) wcentr, ichmin
300		format ('WARNING: Gaussian for center wavelength ',
				f10.5, ' is zero.  ',/,
				'Fixing a delta width for interpolation',
				' at input channel ',i6,
				' Gaussian = 1.0')
		gauss(ichmin) = 1.0
	}

return
end
