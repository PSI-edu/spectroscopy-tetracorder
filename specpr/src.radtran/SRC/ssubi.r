	subroutine Ssubi (xn1, xkm1, s, xn2, xkm2, Se, iflgse )

# Compute the internal reflection coefficient, Se, for a spherical particle

# This subroutine selects which Ssubi (Ssube with parameters reversed) model to use
# depending on 
# flag iflgse to say which Se subroutine to use:
#      = 0      Use the original Hapke formulation, derived from 1981 graph
#      = 1981   Use the original Hapke formulation, derived from 1981 graph
#      = 59     Use Hapke 2012 book  page 59 equations
#      = 60     Use Hapke 2012 book  page 60 equations

# R. Clark 12/19/2013

# Input:
#        xn2   = index of refraction, real part for the material in the medium 1
#        xkm2  = index of refraction, imaginary part for material in the medium 1
#
#        s     = Hapke grain internal scattering factor
#
#        xn1   = index of refraction, real part for the matrix medium (1)
#        xkm1  = index of refraction, imaginary part for the matrix medium (1)

# output:
#        Se    =  medium 2 grain external reflection coefficient


        implicit none

	integer*4 iflgse
	real*4    xn1, xn2, xkm1, xkm2, s, se
	real*4    f_Se_pg59, f_Se_pg60, dx4

	if ( iflgse == 59 ) {    # Use Hapke 2012 book  page 59 equations

		dx4=1.0   # numerical integration step in degrees
                          # NOTE: Si calculation = Se with material 1 and 2 indices reversed
		Se = f_Se_pg59(xn2,xkm2,xn1,xkm1,dx4)

	} else if ( iflgse == 60 ) {  # Use Hapke 2012 book  page 60 equations

		dx4=1.0   # numerical integration step in degrees
                          # NOTE: Si calculation = Se with material 1 and 2 indices reversed
		Se = f_Se_pg60(xn2,xkm2,xn1,xkm1,dx4)

	} else {    # Use the original Hapke formulation, derived from 1981 graph
                    # NOTE: this version assumes the matrix material is air or vacuum
                    # with xn1 = 1.0, xkm1 = 0.0

		call Ssubi1981 (xn2,xkm2,s,Se)
	}
	end
