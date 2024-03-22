   subroutine calcr(j, rfx, r1, xxn, xxk, xemn1,xemk1, gmalb, astep, sd)
#
# 	calculate the reflectance value at one wavelength, channel =j
#
#                        j = index for array for one wavelength 
#                      xxn = index of refraction (at one wavelength)
#                      xxk = absorption coefficient in inverse cm. (at one wavelength)
#                    xemn1 = index of refraction of the matrix (at one wavelength)
#                    xemk1 = absorption coefficient in inverse cm of the matrix (at one wavelength)



#	implicit integer*4 (i-n)
	implicit none

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"

	real*4 xxn(NMINL), xxk(NMINL)	#temp arrays for passing to
					# mrefl subroutine
        real*4 xemn1,xemk1   #  index of refraction and abs coef of the matrix (at one wavelength)
	real*4 rfx, r1

	real*4 gmalb, astep, sd, sum, xangle, xastep

	integer*4 j, jangle, iastep, iastep2, ifl, flag

	currch = 0		#turn off albedo calculation
	flag=0


	do ifl = 1, nminer {
		call dwcomp (ddflt(ifl),df,w0,wav(j),d(ifl))
	}
	if (gmalb == 1.) {    # geometric albedo
                              # numerical integration over spherical body
		sum = 0.
		xastep = astep/57.29578   # integration step size in radians

		iastep2=astep/2.
		iastep=astep

		do jangle = iastep2, 90, iastep {
			xangle = float(jangle)/57.29578
			mu = cos(xangle)
			mu0 = mu
			call mrefl3 (nminer,wav(j),xxn,xxk, xemn1,xemk1,
				d,weight,dens,mu0,mu,rfx,
				wsmean(j),r1,g,wband,currch,sd,inrmlc,flag,iflgse)
			sum = sum + 2*mu*mu0*rfx*sin(xangle)*xastep
		}
		rfx = sum

	} else {    # not geometric albedo

		call mrefl3 (nminer,wav(j),xxn,xxk,  xemn1,xemk1,
			d,weight,dens,mu0,mu,rfx,
			wsmean(j),r1,g,wband,currch,sd,inrmlc,flag,iflgse)
#		write (ttyout,900) j, wsmean(j)
#900		format (' DEBUG calcr: j=',i5,' wsmean(j)=',f12.6)
	}
#
# debug write
#
#	write (ttyout,100) rfx
#100	format(' reflectance = ',e12.4)

500	return
	end
