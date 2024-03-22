      subroutine singscat (Se, Si, aD, sD, D, wav, dp, n, k, mx,
     c                     Qs1, Qs, Qa, Qe, wsingl, ws1st)

            implicit none

            real*4 Se, Si, aD, sD, D, wav, dp, n, k, mx
c### not used             complex*4 mc

            real*4 Qs1, Qs, Qa, Qe, wsingl, ws1st

c local variables:

            real*4 nnkk, nnkk2, nnmkk, Qstop, Qsbot, bot, xtop
	    real*4 xfact, dpw, utest
	    real*4 df, XX, Qd, x, r1, xe, xexp, xq, xqtop, xqbot

c was: subroutine Qsubs (Se,Si,aD,sD,Qs1,Qs)

c WAS:  this subroutine computes equation 24 of Hapke (1981) JGR vol 16
c          pages 3039-3054.    - pre 2009
c
c          plus modfications for diffraction, Rayleigh scattering,
c               and Rayleigh absorption.  
c                                   - RNC 10/10/2009
c
c     input variables:
c           Se = external reflection
c           Si = internal reflection
c           aD = absoprtion coefficient * grain diameter
c           sD = grain internal scattering coeff * grain diameter
c            D = grain diameter in cm
c          wav = wavelength in microns
c           dp = interparticle distance (cm)
c not used  mc = complex index of refraction mc = n + ik  (complex)
c           n  = real part of index of refraction
c           k  = imaginary part of index of refraction
c          mx  = mass fraction
c    
c     computed:
c           Qs = scattering efficiency
c           Qa = absorption efficiency
c           Qe = extinction efficiency
c       wsingl = singlescattering albedo
c        ws1st = first surface reflection (normal)
c
c                note: Se,Si,r1 are >= 0.0 and <= 1.0.
c
c          Qs1 = Qs - Se which is the single scattering
c                             albedo for first order scattering
c                             but not including first surface
c                             reflection from grain surfaces.
c           Qs = the single scattering albedo in Hapke 1981
c                    and assumed Qe = 1.0
c
c     note: absorption coefficient = 4 pi k/wav
c
c                BUT now we do more rigorous:
c
c     more rigorous is 
c
c               Qs = Qe - Qa,  Qe = Qs + Qa
c
c               wsingl = Qs/Qe = Qs/(Qs + Qa) - 1/(1+Qa/Qs)
c
c               Qs = Qd + Qs' = diffraction component + geometric component.
c
c               Previously (pre 2009) we assumed Qd =0
c               Now add Qd =0 when particles touching, growing to 1 when
c               particles separated by a wavelength.
c
c               XX = Hakpe's X parameter = pi * D / wav   (D. wav in same units)
c
c               irregular particles widely separated: Qe = 2 with diffraction.
c
c               distance between particles: Hapke 1993, eqn 10.58, p280
c                    mass of one particle: 4/3 pi r^3 rho
c                                       rho = density.
c                    pv = particles/volume = mx *p/(4/3 pi r^3) 
c                                          = 6 mx *p/(pi D^3) 
c                         mx = mass fraction, p packing density.
c                    dp = distance between particles = (1/pv)^(1/3)
c
c     first compute how much diffraction to add,
c        df = diffraction factor, 0 to 1
c     NOTE: this should be computed outside this routine and passed
c           for increased speed.

      df = dp/D 

      XX = 3.14159265 * D / (wav / 10000.)
      xfact = 3.0*(n-1.0)*XX

c
c     Qs = Qd + Qs'  after Hapke 1993, p94 last line and eq 5.25 p87
c
      if ( df .gt. xfact) then
	Qd = 1.0
      else
	Qd = xfact/df
      endif

c
c According to Hapke (1981 and 1993), for particles touching, Qe=1 and
c there is no diffraction component.  But for small abundance particles,
c in a larger matrix, they are separated enough so that diffraction
c is a factot.  So add the diffraction component back in when
c the average particle spacing is greater than 1 wavelength.  If
c less than 1 wavelength, then decrease the diffraction component
c proportionally.
c
c remember dp is in centimeters, wav in microns, so the 10,000 factor
c

	Qa = 0.0
	Qd = 0.0

	dpw = (dp/10000.0)/wav
	if (dpw < 1.0) then
		Qd = Qd * dpw
	endif
c	write (*,218) df, dp, dpw, xfact, D
c218     format(' df=',e12.6,'   dp=',e12.6,'   dpw=',e12.6,
c     c '   xfact=',e12.6,'   D=',f9.6)
	
c now if particles are really small:
c
c     note: the Qs below has Qd included, so we don't have the
c           above Qd modifications.  Must resolve this in the future.
c           Until then, this code is limited over what will be valid (see Hapke).
c  D < 0.0001 = particles less than 1 micron.

c#whichtouse?      if (xfact < 1.0) then
c#whichtouse?      if (XX < 1.0) then

      if (D < 0.0001) then

		nnkk = n*n + k*k
		nnkk2= (n*n + k*k)**2
		nnmkk= (n*n - k*k)
		bot  = (nnkk2 +4.0*nnmkk+4.0)

c                    eqn 5.13 Hapke 1993, p. 73

		xtop = 24.0*n*k*XX
		Qa = xtop/bot

c                    eqn 5.14 Hapke 1993, p. 73

		Qstop =8.0*(((nnkk2+n*n-k*k-2.0)**2.0)+36.0*n*n*k*k)
		Qsbot =3.0*(bot**2)

c		write (*,180) bot, xtop, Qa, Qstop, Qsbot
c180             format (' DEBUG singscat: bot=',f12.5,
c     c                  ' xtop=',f12.5,' Qa=',f12.5,
c     c                  ' Qstop=',f12.5,' Qsbot=',f12.5)

c                    eqn 5.14 Hapke 1993, p. 73

		Qs = (XX**4)*(Qstop/Qsbot)

		if (XX .lt. 1.0) then

c			write(*,1233) Qs, Qa, XX
c1233      format (' DEBUG singscat Qs=',f12.5,'   Qa=',f12.5,
c     c            '   XX=',f12.5)

			Qs = Qs * XX
			Se = Se * XX
		endif
		Qe = Qs + Qa

		wsingl = Qs / Qe

		ws1st = wsingl

c		write(*,220) wav, n, k, bot, XX, mx
c220   format (' DEBUG singscat wav=',f8.3, ' n=',f8.3 ,'  k=',e12.6 ,
c     c        '  bot=',d14.6 ,
c     c        '   XX=',f12.6, '   wfract=',f8.6)
c		write(*,221) Qs, Qa, Qe, wsingl
c221   format (' DEBUG singscat Qs=', f18.6,' Qa=', f18.6,
c     c        ' Qe=', f18.6, ' wsingl=',f12.6)

      else     

c              this is the old Hapke stuff (Hapke, 1981) using geometric
c              optics (pre 2009)
c              plus the diffraction component added (10/11/2009)
c
               x = (aD/(aD+sD))**0.5
               r1 = (1.0-x)/(1.0+x)
c
c     compute argument to exponent, if overrange set to max value
c
               xe = -0.6666667*(aD*(aD+sD))**0.5

c		write(*,23) aD, sD
c23		format(' aD=',e12.6,'    sD=',e12.6)

               if (xe .lt. -50.0) then
	  		xe = -50.0
			xexp = 0.0
      		else
			xexp = exp(xe)
      		endif
c
c   debug write
c
c	write(6,25) xe, xexp
c 25	format(' xe= ',e10.4,'   xexp= ',e10.4)
c
c debug write
c
c	write(6,35) Se, Si, r1
c35    format(' DEBUG singscat Se= ',f7.4,'  Si= ',f7.4,'  R1 = ',e10.4)
c	utest = (r1-Si)*xexp
c	write(6,45) utest
c45	format(' DEBUG singscat Test for underflow',e10.4)

c was           xq = ((1.0-Se)*(1.0-Si)*(r1+xexp))
c    1               /(1.0 - r1*Si + (r1-Si)*xexp)

		xqtop = ((1.0-Se)*(1.0-Si)*(r1+xexp))
		xqbot = (1.0 - r1*Si + (r1-Si)*xexp)

		if (xqbot .lt. 0.1e-15) xqbot = 0.1e-15

		xq = xqtop / xqbot

c		write (6,*) ' DEBUG singscat xqtop=',xqtop,
c     1                      ' xqbot=',xqbot,' xq=',xq

                Qs = Se + xq

      		Qs1 = Qs - Se
c
c      		write (6,*) ' DEBUG singscat Qs=', Qs,'  xq=',xq 

		wsingl = (Qs + Qd)/(1.0 + Qd)

c    ws1st is the first surface reflection from grains plus the grain internal
c          scattering.  RNC 10/16/2009

		ws1st = Qs1 + ((n-1.0)**2 + k*k)/((n+1.0)**2 + k*k)

c		write (6,66) wav, n, k, ws1st
c66     format (' DEBUG singscat  wav=',f8.3,'  n=',f9.5,
c     6         '  k=',f9.5,'  ws1st=',f9.5)


      endif

c      write (6,222) wav, Qs, Qd, Qa, Qs1, wsingl, mx
c222   format (' DEBUG singscat wav=',f8.3, ' Qs=',f9.5 ,
c     6        '  Qd=',f9.5 ,'  Qa=',f9.5 ,
c     6        '   Qs1=',f9.6,'  wsingl=',f9.6,'   wfract=',f8.6)

      return
      end
