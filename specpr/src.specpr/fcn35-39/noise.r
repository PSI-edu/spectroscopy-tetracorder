	subroutine noise(xnoise)
	implicit none
#ccc  name:  noise
#ccc  version date:  July 10, 1990
#ccc  author(s):  Gregg A. Swayze
#ccc  language:  Ratfor
#ccc
#ccc  short description: Retruns a normally distributed random
#ccc  number with zero mean and unit variance, using random as 
#ccc  the random number generator. 
#ccc
#ccc  algorithm description see Press, W.H., Flannery, B.P.,
#ccc  Teukolsky, S.A., and Vetterling, W.T., 1986, Numerical
#ccc  Recipes: The art of scientific computing: Cambridge University
#ccc  Press, p. 818.ZZ 
#ccc  system requirements:
#ccc  subroutines called:
#ccc
#ccc
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
      real*4 xnoise,r,v1,v2,fac,gset,x1,x2
	  integer *4 iset
	  DATA iset/0/
  DATA gset/0.0/

      r = 3
      if (iset == 0) {
        while (r >= 1) {
		call frandom(x1)
		call frandom(x2)
            v1=2.*x1-1.
            v2=2.*x2-1.
            r=v1**2+v2**2
        }
        fac=sqrt(-2.*log(r)/r)
        gset=v1*fac
        xnoise=v2*fac
        iset=1
      } else {
        xnoise=gset
        iset=0
      }
      return
      end
