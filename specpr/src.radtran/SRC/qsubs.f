c     this subroutine computes equation 24 of Hapke (1981) JGR vol 16
c          pages 3039-3054.
c
c     input variables:
c           Se = external reflection
c           Si = internal reflection
c           aD = absoprtion coefficient * grain diameter
c           sD = grain internal scattering coeff * grain diameter
c
c     note: Se,Si,r1 are >= 0.0 and <= 1.0.
c
c     computed: Qs1 = Qs - Se which is the single scattering
c                             albedo for first order scattering
c                             but not including first surface
c                             reflection from grain surfaces.
c               Qs = the single scattering albedo 
c
      subroutine Qsubs (Se, Si, aD, sD, Qs1, Qs)
      implicit none

      real*4 Se, Si, aD, sD, Qs1, Qs
      real*4 xe, r1, x, xexp
c
      x = (aD/(aD+sD))**0.5
      r1 = (1.0-x)/(1.0+x)
c
c     compute argument to exponent, if overrange set to max value
c
      xe = -0.6666667*(aD*(aD+sD))**0.5
      if (xe.lt.-80.0) then
		xe = -80.0
		xexp = 0.0
      else
		xexp = exp(xe)
      endif
c
c   debug write
c
c	write(6,25) xe, xexp
c 25	format('xe= ',f8.1,'   xexp= ',e10.4)
c
c debug write
c
c	write(6,35) Se, Si, r1
c 35	format('S sub e= ',f7.4,'  S sub i= ',f7.4,'  R1 = ',e10.4)
c	utest = (r1-Si)*xexp
c	write(6,45) utest
c 45	format('Test for underflow',e10.4)

      Qs = Se + ((1.0-Se)*(1.0-Si)*(r1+xexp))
     1            /(1.0 - r1*Si + (r1-Si)*xexp)


      Qs1 = Qs - Se
c
c      write (6,*) 'Q sub s = ', Qs

      return
      end
