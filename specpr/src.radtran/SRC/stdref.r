	subroutine stdref (mu0,mu,r)

	implicit integer*4 (i-n)
#
#     this program computes the reflectance of a standard surface
#	at a given viewing geometry B(g)=0, P(g)=1, w=1
#
#     input :
#                      mu0 = cosine of angle of incidence
#                       mu = cosine of angle of emission
#
#     output:
#               r = reflectance
#
#**************************************************************************
#
#
	integer*4 currch

      real*4 mu,mu0, mu4
#
# reflectance computed using hapke eqn 16, for an isotropic case
#        B(g) = 0.0, P(g)= 1.0 and mu = mu0 = 1.0
#        subroutine hr = Chandrasekhar H function.
#
	w = 1.0
	wxmean = 0.0
	call hr (mu,w,wxmean,Hmu)
	call hr (mu0,w,wxmean,Hmu0)
	mu4 = 4.0 * (mu+mu0)
#
	r = mu0*(Hmu*Hmu0)/mu4

##	write (6,500) r, Hmu, Hmu0, mu, mu0
##500	format('DEBUG: reflectance standard = ',f9.7, 4(2x, f7.5))
#
100	return
	end
