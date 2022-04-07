	subroutine convol(outchn,nchans,t,r,x,s,c,nmode,
				imode,iline,isamp,tlim)
	implicit integer*4 (i-n)


#ccc  name: convol
#ccc  version date: 
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description: calculates the output spectrum and center
#ccc  			wavelengths
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc				outchn: the output channel
#ccc				s     : array of the output spectrum
#ccc				c     : array of the output center values    
#ccc				t     : array of input spectral bandpasses
#ccc				r     : array of input spectrum
#ccc				x     : array of input wavelengths  
#ccc				nmode : normalize mode
#ccc				imode : imaging mode diagnostics
#ccc						= 0 not imaging
#ccc						= 1 imaging mode no messages
#ccc						= 2 imaging mode messages
#ccc				iline : image line number
#ccc				isamp : image sample number
#ccc				tlim  : limit on bandpass fcn, below which
#ccc                                    consider = 0.0, and no effect
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc





	include "../common/lundefs"

#RED
	real*4 delx     # function delx

	real*4 s(nchans),c(nchans),r(nchans)
	real*4 x(nchans),t(nchans),tlim
	real*4 nrmsum,tmpnrm
	integer*4 outchn,nchans

#
#     *** calculate center values and new output spectrum ***
#
300 	s(outchn)=0.0
		cx=0.0
	nrmsum=0.0
		do j=1, nchans {
#              *** check for bad data in files ***
			if (x(j)==-1.23e34 || r(j)==-1.23e34 || 
						t(j)==-1.23e34) next
			if (t(j) < tlim) {
				#t(j)=0.0   # guard against underflow
				next
			}
			z = delx (x,t,r,j,nchans)
			if (abs(z) > 1.0e10) {
				if (imode == 0) write (ttyout,400) j, z, outchn
400				format (' ERROR in convol:',
					' input channel spacing too large:',
					' channel',i6,' spacing:',
					1pe12.4,/,
					' OUTPUT channel',i6,' deleted')
				s(outchn) = -1.23e34
				c(outchn) = -1.23e34
				return
			} else {

				xx = t(j)*z
				y=r(j)*xx
#DEBUG				if(outchn == 58) {
#DEBUG					write(6,*)'DEBUG: j=',j,
#DEBUG						'  t=',t(j),' xx=',xx,'  z=',z,
#DEBUG						'  r=',r(j),'  y=',y
#DEBUG				}
				s(outchn)=s(outchn)+y
				if (nmode==1)  {
					tmpnrm=xx
					nrmsum=nrmsum+tmpnrm
				}
				cx=cx+x(j)*y
			}
		}
		if (nmode == 1) {
			if (nrmsum > 0) {
				s(outchn)=s(outchn)/nrmsum
			} else {
				if (imode == 0) write (ttyout, 500) outchn
500				format (' ERROR in convol: normalization',
					' factor is zero.  Output channel',
					i6,' deleted')
				s(outchn)=-1.23e34
				c(outchn)=-1.23e34
				return
			}
		}
		if (s(outchn) == 0.0) {
			if (imode == 0) write(ttyout,335) outchn
			s(outchn)=-1.23e34
			c(outchn) = -1.23e34
			return
		}
		if (abs(s(outchn)) < 1.0e15) {  # be sure no overflow
			c(outchn)=cx/s(outchn)
		} else {
			c(outchn) = -1.23e34
		}

335     format (/,
' *** warning ***: division by zero ***',/,
' channel no. ',i4,' in output spectrum has been reset to -1.23e34.',/,
'data will be affected.',/)

return
end
