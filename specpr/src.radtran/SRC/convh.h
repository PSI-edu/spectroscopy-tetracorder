#
#    Whenever this header file is used, source should also include defs.h

# definitions:
#
# ibandn: channel numbers to define band: 1, 3= continuum, 2= band center
# naband: number of absorption bands defined (0 to NBND)
# banddp: computed band depth for each band defined by ibandn
# contrf: computed continuum reflectance for each band defined by ibandn
# xbandd: comparison spectrum band depths defined by ibandn
# xcontr: comparison spectrum continuum reflectance defined bi ibandn
# saturd: grain size saturation point for each band defined by ibandn
# wband:  computed single scattering albedo for each band at three
#		channels given by ibandn
# wbandc: computed single scattering albedo at continuum (averaged from
#		continuum channels defined in ibandn)
# wibndc: single scattering albedo for each mineral at band center channel
#		defined by ibandn to compare grain darkness of each mineral
# iwdirb: direction to step weight at a particular band
# iddirb: direction to step d at a particular band
#
# direction definitions:
#		<0: decrease
#		 0: undetermined
#		>0: increase
#
# wdir: overall direction to step weight for a particular mineral
# ddir: overall direction to step d for a particular mineral
# maxdnc: pointer to mineral with maximum wbandc for a particular absorption
# mindnc: pointer to mineral with minimum wbandc for a particular absorption
# currch: current channel number

	common /convh/ ibandn(NMINL,NBND,3)
	common /convh/ naband(NMINL)
	common /convh/ banddp(NMINL,NBND)
	common /convh/ contrf(NMINL,NBND)
	common /convh/ xbandd(NMINL,NBND)
	common /convh/ xcontr(NMINL,NBND)
	common /convh/ saturd(NMINL,NBND)
	common /convh/ wband (SPMAXCHAN,NMINL)
	common /convh/ wbandc(NMINL,NBND)
	common /convh/ iwdirb(NMINL,NBND)
	common /convh/ iddirb(NMINL,NBND)
	common /convh/ wdir(NMINL),ddir(NMINL)
	common /convh/ maxbnc(NBND),minbnc(NBND)
	common /convh/ wibndc(NMINL,NBND), currch

	integer*4 ibandn,naband,iwdirb,iddirb,maxbnc,minbnc,currch
	real*4    bandp, contrf,xbandd,xcontr,saturd,wbandc,wband,wibndc
	real*4    banddp
	integer*4    wdir,ddir
