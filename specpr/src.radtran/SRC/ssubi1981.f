      subroutine Ssubi1981 (xn, xk, s, Si)
      implicit none

c  Computes the internal reflection coefficient from a spherical particle.

c  This subrouting written with the original radtran in the 1980's
c  by Roger N. Clark, using parameterization of the graph of Se
c  in Hapke, 1981, JGR

c  input:
c         xn
c         xk
c         s   (but s not used)

c  computed:  Se


      real*4 xn, xk, s, Si
c
      real*4 xsave
      real*4 offset

      xsave=xn
      if (xn.le.1.0) xn=1/xn
c
c     xn > 5.0
     
      if (xn.lt.5.0) goto 4
      Si = .97 + (xn-5.0)*.001
      if (Si.gt.1.0) Si=1.0
      xn=xsave
      return

c     xn > 4.0

4     if (xn.lt.4.0) go to 6
      Si = .96 + (xn -4.0)*.01 
      xn=xsave
      return

c     xn > 3.0
c

6     if (xn.lt.3.0) go to 8
      Si = 0.9 + (xn-3.0)*.06
      xn=xsave
      return
c
c     xn > 2.0

8     if (xn.lt.2.0) go to 10
      Si = 0.8 + (xn-2.0)*0.1
      xn=xsave
      return
c
c
c     1.5 < xn < 2.0
c
10    if (xn.lt.1.5) go to 20
      Si = 0.6 + (xn-1.5)*0.4
      xn=xsave
      return
c

c
c     1.25 < xn < 1.5
c
20    if (xn.lt.1.25) go to 30
      Si = 0.4 + (xn-1.25)*0.8
      xn=xsave
      return
c

c
c     xn < 1.25
c
30    Si = ((xn-1.0)*1.6 )
      xn = xsave
      return
c

      end      
