   subroutine calcrlyr(jlyr, j, rfx, r1, xxn, xxk, xemn1,xemk1, gmalb, astep, sd)
#
# 	calculate the reflectance value at one wavelength, channel =j
#
#                     jlyr = layer number
#                        j = index for array for one wavelength 
#                      xxn = index of refraction (at one wavelength)
#                      xxk = absorption coefficient in inverse cm. (at one wavelength)
#                    xemn1 = index of refraction of the matrix (at one wavelength)
#                    xemk1 = absorption coefficient in inverse cm of the matrix (at one wavelength)
#                    gmalb = 1 means compute geometric albedo, using step size astep

# computed:
#
#   rfx    = computed reflectance  for the mixture, and layer
#   r1     = computed first surface reflectance for the mixture, and layer

# computes, but in lmreflh
#   wsmean = mean single scattering albedo for the mixture, and layer        ADD





#	implicit integer*4 (i-n)
	implicit none

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"

	integer*4 jlyr

	real*4 xxn(NMINL), xxk(NMINL)	#temp arrays for passing to
					# mrefl subroutine
        real*4 xemn1,xemk1   #  index of refraction and abs coef of the matrix (at one wavelength)
	real*4 rfx, r1

	real*4 gmalb, astep, sd, sum, xangle, xastep

	integer*4 j, jangle, iastep, iastep2, ifl, flag

	currch = 0		#turn off albedo calculation
	flag=0

	nminer = nminerlyr(jlyr)   # working number of minerals for this layer


	do ifl = 1, nminer {   # reorganize arrays for sending to mrefl3lyr

		# grain size dependent parameter d = ddflt/(1.0 + df * (w0 - wavelength))
		call dwcomp (ddfltlyr(jlyr, ifl), df, w0, wav(j), d(ifl))
		dlyr(jlyr, ifl) = d(ifl)

		# computed by mrefl3lyr: wsmean(j), 
	}

	#
	# currch: current channel number (conv.h)
	# wband:  computed single scattering albedo for each band at three channels (conv.h)

	# for the layer calculations, we need (using Clark and Roushm JGR, 1984, =C&R)

	# lbarprime = (-1 / abscoef) * ln(wsmean) (C&R eqn 18) = mean optical path in a single grain
	# m         = ln(rfx) / ln(wsmean)        (C&R eqn 19) = mean number of particles encountered by photons
	# lbar      = m * lbarprime               (C&R eqn 20) = mean optical path length = mopl
	# mplt      = (pi/4) * ds * sqrt(m/2)     (C&R eqn 24) = mean penetrated layer thickness = optical depth
	#                      ds = mean distance between particles.
	# argue: ds = mean grain diameter, dmean, * some surface density factor, but = dmean for close packing.


	if (gmalb == 1.) {    # geometric albedo
                              # numerical integration over spherical body, steps = astep
		sum = 0.
		xastep = astep/57.29578   # integration step size in radians

		iastep2=astep/2.
		iastep=astep

		do jangle = iastep2, 90, iastep {
			xangle = float(jangle)/57.29578
			mu = cos(xangle)
			mu0 = mu
			call mrefl3lyr (jlyr, nminer,wav(j), xxn, xxk, xemn1, xemk1,
				d, weight, dens, mu0, mu, rfx,
				wsmean(j), r1, g, wband, currch, sd, inrmlc, flag, iflgse)
			sum = sum + 2*mu*mu0*rfx*sin(xangle)*xastep
		}
		rfx = sum   # rfx = final reflectance for this layer

	} else {    # not geometric albedo

		call mrefl3lyr (jlyr, nminer, wav(j), xxn, xxk,  xemn1, xemk1,
			d, weight, dens, mu0, mu, rfx,
			wsmean(j), r1, g, wband, currch, sd, inrmlc, flag, iflgse)

		# rfx = final reflectance for this layer
		wsmeanlyr(jlyr, j) = wsmean(j)  # mean single scattering albedo, this layer, channel.

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
