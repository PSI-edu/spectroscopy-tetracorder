# Whenever this header file is used, source should also 
# include defs.h BEFORE this file

#
#
# parameter (NBND = 3)        # in defs.h
# parameter (NMINL = 20)      # in defs.h
# parameter (MAXCHNS = 4852)  # in defs.h  ### OLD now use SPMAXCHAN
# parameter (MAXTEXT = 19408) # in defs.h  ### OLD now use SPMAXCHAN
# parameter (NAREF = 2)       # in defs.h
# parameter (NMMIX = 5)       # in defs.h
# parameter (MAXLAYERS = 3)     # in defs.h  number of layers in layered surfaces


	common /lmrefl/ dmu, dmu0, dmu0a
	common /lmrefl/ wav, d, ddflt, weight, dens, scoef
	common /lmrefl/ xn, xk, xkm, xemn, xemk, xemkm
	common /lmrefl/ xvmn, xvmk, xvmkm, xi, r
	common /lmrefl/ rfirst, rtop, rsecnd, rthird, rfourth, rsixth
	common /lmrefl/ irecxn, irecxk, irecxen, irecxek, irecxi
	common /lmrefl/ xiangl, xeangl, wsmean, g
	common /lmrefl/ idxn,idxk,idxen, idxek, idxi
	common /lmrefl/ mu, mu0, xse, xsi
	common /lmrefl/ nminer, imask, npeaka,ipeaka 
	common /lmrefl/ dlowlm, duplm, wlowlm
	common /lmrefl/ wuplm, wpf, dpf, df, w0, inrmlc, matrixptr, iflgse
	common /lmrefl/ xntitl, xktitl, xentitl, xektitl,xititl
	common /lmrefl/ nlayers, xkmeanlyr, ddfltmeanlyr
	common /lmrefl/ dlyr, ddfltlyr, weightlyr, denslyr, scoeflyr
	common /lmrefl/ xnlyr, xklyr, xkmlyr, xemnlyr, xemklyr, xemkmlyr
	common /lmrefl/ rlyr, rfirstlyr, wsmeanlyr, mparticlyr
	common /lmrefl/ tlyr, odlyr, pdepthlyr, mopllyr
	common /lmrefl/ irecxnlyr, irecxklyr, idxnlyr, idxklyr
	common /lmrefl/ xselyr, xsilyr, nminerlyr, npeakalyr, ipeakalyr
	common /lmrefl/ xntitllyr, xktitllyr

	integer*4 nlayers    # number of layers in use

	real*8    dmu                      # double precision cosine ange of emission
	real*8    dmu0                     # double precision cosine ange of incidence
	real*8    dmu0a(100)               # double precision cosine ange of incidence array for numerical integrations
	real*4    wav(SPMAXCHAN)           # wavelengths in microns
	real*4    d(NMINL)                   # grain diameter in centimeters
	real*4    dlyr(MAXLAYERS,NMINL)      # grain diameter in centimeters for each mineral in each layer
	real*4    ddflt(NMINL)               # grain size in cm
	real*4    ddfltlyr(MAXLAYERS,NMINL)  # grain size in cm
	real*4    ddfltmeanlyr(MAXLAYERS)    # mean grain size in cm for layer
	real*4    weight(NMINL)              # material weight fraction
	real*4    weightlyr(MAXLAYERS,NMINL) # material weight fraction
	real*4    dens(NMINL)                # material density
	real*4    denslyr(MAXLAYERS,NMINL)   # material density
	real*4    scoef(NMINL)               # Hapke grain internal scattering coefficient
	real*4    scoeflyr(MAXLAYERS,NMINL)  # Hapke grain internal scattering coefficient

	real*4    xn(NMINL,SPMAXCHAN)               # index of refraction for intimate mix component NMINL (real part)
	real*4    xnlyr(MAXLAYERS,NMINL,SPMAXCHAN)  # index of refraction for intimate mix component NMINL (real part)
	real*4    xk(NMINL,SPMAXCHAN)               # absorption coefficient for intimate mix component NMINL (in cm^-1)
	real*4    xklyr(MAXLAYERS,NMINL,SPMAXCHAN)  # absorption coefficient for intimate mix component NMINL (in cm^-1)
	real*4    xkmeanlyr(MAXLAYERS,SPMAXCHAN)    # mean absorption coefficient for layer (in cm^-1)
	real*4    xkm(NMINL,SPMAXCHAN)              # absorption coefficient for intimate mix component NMINL (imaginary part)
	real*4    xkmlyr(MAXLAYERS,NMINL,SPMAXCHAN) # absorption coefficient for intimate mix component NMINL (imaginary part)
                                                    #    as in index of refraction = n+i*m = xn + i* xkm

	real*4    xemn(SPMAXCHAN)                # index of refraction for matrix that particles are embedded in
	real*4    xemnlyr(MAXLAYERS,SPMAXCHAN)   # index of refraction for matrix that particles are embedded in
	real*4    xemk(SPMAXCHAN)                # absorption coefficient for matrix that particles are embedded in
	real*4    xemklyr(MAXLAYERS,SPMAXCHAN)   # absorption coefficient for matrix that particles are embedded in
	real*4    xemkm(SPMAXCHAN)               # absorption coefficient (imaginary part) for matrix that particles are embedded in
	real*4    xemkmlyr(MAXLAYERS,SPMAXCHAN)  # absorption coefficient (imaginary part) for matrix that particles are embedded in

	real*4    xvmn(SPMAXCHAN)         # index of refraction for vacuum (=1.0) or air
	real*4    xvmk(SPMAXCHAN)         # absorption coefficient for vacuum  (=0.0)or air
	real*4    xvmkm(SPMAXCHAN)        # absorption coefficient (imaginary part) for vacuum  (=0.0)or air

	real*4    xi(SPMAXCHAN)                   # reference comparison spectrum
	real*4    r(SPMAXCHAN)                    # final computed intimate mix reflectance spectrum
	real*4    rlyr(MAXLAYERS,SPMAXCHAN)       # final computed intimate mix reflectance spectrum
	real*4    rfirst(SPMAXCHAN)               # first surface reflection from intimate mix calculation
	real*4    rtop(SPMAXCHAN)                 # top layer reflectance component of layered reflectance.
	real*4    rsecnd(SPMAXCHAN)               # 2nd order component of layered reflectance.
	real*4    rthird(SPMAXCHAN)               # 3rd order component of layered reflectance. should be small
	real*4    rfourth(SPMAXCHAN)              # 4th order component of layered reflectance.  Should be ~ 0
	real*4    rsixth(SPMAXCHAN)               # 6th order component of layered reflectance.  Should be ~ 0
	real*4    rfirstlyr(MAXLAYERS,SPMAXCHAN)  # first surface reflection from intimate mix calculation
	real*4    tlyr(MAXLAYERS)                 # layer thickness
	real*4    odlyr(MAXLAYERS,SPMAXCHAN)      # layer optical depth vs wavelength
	real*4    pdepthlyr(MAXLAYERS,SPMAXCHAN)  # penetration depth, 1/e depth vs wavelength for infinite thiickness
	real*4    mopllyr(MAXLAYERS)              # layer mean optical depth length (for an infinite thickness layer)
	real*4    mparticlyr(MAXLAYERS,SPMAXCHAN) # mean number of particles encountered by photons in layer
	integer*4 irecxn(NMINL)               # record number for index of refractions in intimate mix
	integer*4 irecxnlyr(MAXLAYERS,NMINL)  # record number for index of refractions in intimate mix
	integer*4 irecxk(NMINL)               # record number for absorption coefficients
	integer*4 irecxklyr(MAXLAYERS,NMINL)  # record number for absorption coefficients
	integer*4 irecxen                 # record number for index of refraction for matrix material
	integer*4 irecxek                 # record number for absorption coefficients for matrix material
	integer*4 irecxi                  # record number for xi set
	real*4    xiangl                  # angle of incidence
	real*4    xeangl                  # angle of emission
	real*4    wsmean(SPMAXCHAN)              # mean single scattering albedo of a multicomponent mixture
	real*4    wsmeanlyr(MAXLAYERS,SPMAXCHAN) # mean single scattering albedo of a multicomponent mixture
	real*4    g                         # angle used in single particle phase function
	integer*4 idxn(NMINL)               # device letter for index of refraction spectra
	integer*4 idxnlyr(MAXLAYERS,NMINL)  # device letter for index of refraction spectra
	integer*4 idxk(NMINL)               # device letter for absorption coefficients spectra
	integer*4 idxklyr(MAXLAYERS,NMINL)  # device letter for absorption coefficients spectra
	integer*4 idxen                   # device letter for index of refraction for matrix material
	integer*4 idxek                   # device letter for absorption coefficient for matrix material
	integer*4 idxi                    # device letter for  xi set
	real*4    mu                      # cosine ange of emission
	real*4    mu0                     # cosine ange of incidence
	real*4    xse(NMINL,SPMAXCHAN)              # S sub e (Se) for each compound
	real*4    xselyr(MAXLAYERS,NMINL,SPMAXCHAN) # S sub e (Se) for each compound
	real*4    xsi(NMINL,SPMAXCHAN)              # S sub i (Si) for each compound
	real*4    xsilyr(MAXLAYERS,NMINL,SPMAXCHAN) # S sub i (Si) for each compound
	integer*4 nminer                          # number of minerals (working variable)
	integer*4 nminerlyr(MAXLAYERS)            # number of minerals in each layer
	integer*4 imask(SPMAXCHAN)                # mask =0 ignore that channel
	integer*4 npeaka(NMINL)                   # absorption coeff peaks
	integer*4 npeakalyr(MAXLAYERS,NMINL)      # absorption coeff peaks per layer
	integer*4 ipeaka(NMINL,10)                # absorption coeff peaks
	integer*4 ipeakalyr(MAXLAYERS,NMINL,10)     # absorption coeff peaks
	real*4    dlowlm(NMINL)      #   ?? unused?
	real*4    duplm(NMINL)       #   ?? unused?
	real*4    wlowlm(NMINL)      #   ?? unused?
	real*4    wuplm(NMINL)       #   ?? unused?
	real*4    wpf(NMINL)         #   ?? unused?
	real*4    dpf(NMINL)         #   ?? unused?
	real*4    df                 # grain size dependent parameter d = ddflt/(1.0 + df * (w0 - wavelength))
	real*4    w0                 # grain size dependent parameter d = ddflt/(1.0 + df * (w0 - wavelength))
	integer*4 inrmlc             # =1 then normalize the computation to ideal reflectance
	integer*4 matrixptr          # pointer to which optical index set (1 to NMINL) is the matrix material
                                     #    =0 when no matrix
	integer*4 iflgse             # flag to say which Se subroutine to use:
                                     #      = 0      Use the original Hapke formulation, derived from 1981 graph
                                     #      = 1981   Use the original Hapke formulation, derived from 1981 graph
                                     #      = 59     Use Hapke 2012 book  page 59 equations
                                     #      = 60     Use Hapke 2012 book  page 60 equations

	character*40 xntitl(NMINL)              # titles for index of refractions sets
	character*40 xntitllyr(MAXLAYERS,NMINL) # titles for index of refractions sets
	character*40 xktitl(NMINL)              # titles for absorption coefficient sets
	character*40 xktitllyr(MAXLAYERS,NMINL) # titles for absorption coefficient sets
	character*40 xentitl         # title for index of refraction of matrix
	character*40 xektitl         # title for absorption coefficient of matrix
	character*40 xititl          # title for comparison reference spectrum xi



######   areal fractions and molecular mixes

        common /lblref/ xaref, xafrac,  numaref, xatspec
        common /lblref/ xmindx, xmabsc, xmabsclyr, xmolfra, xmolfralyr
	common /lblref/ nmolmix, nmolmixlyr

        real*4    xaref(NAREF,SPMAXCHAN)         # areal mix spectra
	real*4    xafrac(NAREF)                  # areal fractions to each areal mix spectrum
	integer*4 numaref                        # number of areal mix spectra in use
	real*4    xatspec(SPMAXCHAN)             # total of areal mix spectra, areal mix fractions only
	real*4    xmindx(NMMIX,NMINL,SPMAXCHAN)               # index of refraction working array for molecular mix
	real*4    xmindxlyr(MAXLAYERS,NMMIX,NMINL,SPMAXCHAN)  # index of refraction working array for molecular mix
	real*4    xmabsc(NMMIX,NMINL,SPMAXCHAN)               # absorption coefficient working array for molecular mix
	real*4    xmabsclyr(MAXLAYERS,NMMIX,NMINL,SPMAXCHAN)  # absorption coefficient working array for molecular mix
	real*4    xmolfra(NMMIX,NMINL)                        # molecular fractions in each intimate mix component (NMINL)
	real*4    xmolfralyr(MAXLAYERS,NMMIX,NMINL)           # molecular fractions in each intimate mix component (NMINL)
	integer*4 nmolmix(NMINL)                 # number of components used in molecular mix for
				                 #    each compound (NMINL) in the intimate mix
	integer*4 nmolmixlyr(MAXLAYERS,NMINL)    # 
