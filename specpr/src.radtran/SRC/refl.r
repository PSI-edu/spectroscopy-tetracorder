	subroutine refl (iminer,w,xn,xk,xemn1,xemk1,d,weight,dens,mu0,mu,r,
						wsmean,g,iflgse)
	implicit integer*4 (i-n)
#
#     this program computes the reflectance of a particulate surface
#     given the wavelength, the material index of refraction,
#     the absorption coefficient, and a grain size.
#     BUT it only computes reflectance for a single pure mineral.
#         the mineral computed is given by iminer
#
#     input :
#                   iminer = mineral to compute
#                       w  = wavelength in microns
#                       xn = index of refraction
#                       xk = absorption coefficient in inverse cm.
#                    xemn1 = index of refraction of the matrix (at one wavelength)
#                    xemk1 = absorption coefficient in inverse cm of the matrix (at one wavelength)
#                        d = grain size in cm.
#                   weight = weight fraction of each component
#                     dens = density of each component
#                      mu0 = cosine of angle of incidence
#                       mu = cosine of angle of emission
#      iflgse to say which Se subroutine to use:
#      = 0      Use the original Hapke formulation, derived from 1981 graph
#      = 1981   Use the original Hapke formulation, derived from 1981 graph
#      = 59     Use Hapke 2012 book  page 59 equations
#      = 60     Use Hapke 2012 book  page 60 equations
#
#     output:
#               r = reflectance
#               wsmean = single scattering albedo
#
#**************************************************************************
#
#
	real*4 d(iminer), weight(iminer)
      real*4 xn(iminer), xk(iminer), xemn1, xemk1
      real*4 dens(iminer)

# was      real*4 ws1(9), ws(9), s(9)
      real*4 ws1(iminer), ws(iminer), s(iminer)   # changed 6/25/2018
#

      real*4 mu,mu0,mu4
      real*4 ws1st
      integer*4 iminer, iflgse

      pi  = 3.14159265
#
	i = iminer
		if ((xn(i).le.0.1e-30).or.(xk(i).le.0.1e-30)) go to 50
#
		x= xk(i)*(w/10000.)/(4.0*pi*xn(i))
		s(i) = ((xn(i)-1.0)**2 +
			(xn(i)*x)**2)/((xn(i)+1)**2 +
				(xn(i)*x)**2)
#
		call Ssube (xemn1,xemk1,s(i),xn(i),xk(i),Se,iflgse)
		call Ssubi (xemn1,xemk1,s(i),xn(i),xk(i),Si,iflgse)
		sd= 0.3
		xkd = xk(i) * d(i)

# old way:		call Qsubs (Se,Si,xkd,sd,ws1(i),ws(i))
# new way, 2009:
                # imaginary part of the index of refraction:

                xxk = xk(i)*w /(40000.0* pi)  # future: compute elsewhere so not repeat

                p=0.5 # p=packing density: future: add as a user variable

                dp= (1.0/(12.0* weight(i) *p/(pi* d(i)**3)))**(1/3.0)

                call singscat (Se, Si, xkd, sd, d(i), w, dp, xn(i), xxk, weight(i),
                           ws1(i), Qs, Qa, Qe,  ws(i), ws1st)


#
		if (ws(i).gt.1.0) ws(i)=1.0
		if (ws1(i).gt.1.0) ws1(i)=1.0
#
#
# compute mean single scattering albedo
#
	wsmean = ws(i)
	ws1stmean = ws1st
#
# reflectance computed using hapke eqn 16
#        subroutine hr = Chandrasekhar H function.
#
	wxmean = (1.0 - wsmean) **0.5
	call hr (mu,wsmean,wxmean,Hmu)
	call hr (mu0,wsmean,wxmean,Hmu0)
	Bg = 0.0
	b=Se/wsmean
	if (b > 1.0) b = 1.0   # needed? RNC 10/15/2009
	Pg = 1.0 + b*cos(g)    # pre 11/03/2009

	Pg = 0.5    # RNC 11/03/2009  to fit lab ice data

	mu4 = 4.0 * (mu+mu0)
#
	r1 = ws1stmean*mu0*(1.0+Bg)*Pg/mu4
	rm = wsmean*mu0*(Hmu*Hmu0-1.0)/mu4
#
#
	r = r1 + rm 
#
#**debug write statement
#**      write (6,10)  r, r1, rm, rw1, rw1m, wsmean, 
#**     c               Hmu, Hmu0
#**10    format (' r=', 8(f7.4))

	go to 100
#
50	r = -1.23e34
#
100	return
	end
