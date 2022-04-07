      real function f(nwhat, x, param, codeb, codeg)
#----#unction corrected on may 3, 1971 to prevent indef. oper.
#    in floating point operat. due to zero to the zero factor in the
#    computation of the base line.
#
#   dictionary of codes.
#
#    codeb=0       no base line fit,
#    codeb=1,2,3.. a one parameter, two parameter, etc. base line.
#    codeg=0       no gaussian fit
#    codeg=1,2,3.. one, two, etc. gaussians.
#    codeb and codeg may appear in any combination except
#                  for codeb=codeg=0 or some code number
#                  in excess of the program dimension definitions.
#
#
#  nwhat specifies derivatives respect to various para-
#  meters except nwhat=codeb + 3*codeg + 1 that asks
#  for the fitting function itself.
#  nparam(index) are ordered as index=1, 2, 3, 4 etc) to be
#   xline, xcent, delta, xline, etc. and the base line off-
#  set, base line first term in x and so on.
#
      implicit integer*4 (i-n)
      dimension param(66), e(20), w(20)
      integer*2 codeb, codeg
	  data	delete/-1.23e34/
#---compute the codeg gaussians and their derivatives.
	if (x==delete) {
		f = delete
		return
	}
      auxg=0.00
      if(codeg.eq.0) go to 500
#--compute the function if nwhat=nparam + 1
      if(nwhat.ne.(3*codeg+codeb+1)) go to 20
#--compute the  the line constants.
      do l=1, codeg   {
      		index=3*l
      		e(l)=(x-param(index-1))/param(index)
		wcheck= -2.7725887*e(l)*e(l)
		if (wcheck <= -85.) wcheck = -85.
		w(l) = exp(wcheck)
      }
      go to 900
#  if nwhat is less than or equal to codeg, calling program
#  is asking for a gaussian derivative. more precisely
#        nwhat=1, 4, 7.. compute derivative respect to tline,
#        nwhat=2, 5, 8.. compute derivative respect to xcent, and
#        nwhat=3, 6, 9.. compute derivative respect to delta line.
#--find out what calling program really wants.
20   if (nwhat-3*codeg.lt.0) go to 150
     if (nwhat-3*codeg.eq.0) go to 150
     if (nwhat-3*codeg.gt.0) go to 500 
#  statements 150, 150 are to compute derivatives, statement
#  500 is for the function or base line derivatives.
150   nderiv=mod(nwhat,3)
      if(nderiv.eq.0) nderiv=3
#--find for what gaussian is the derivative sought.
      ngauss=(nwhat+2)/3
      index=3*ngauss
      e(ngauss)=(x-param(3*ngauss-1))/param(3*ngauss)
      wcheck= -2.7725887*e(ngauss)*e(ngauss)
      if (wcheck <= -85.) wcheck = -85.
      w(ngauss) = exp(wcheck)
#--compute the nderiv-th for the ngauss-th gaussian.
#40 go to (200, 220, 240), nderiv
40    if (nderiv.eq.1) go to 200
      if (nderiv.eq.2) go to 220
      if (nderiv.eq.3) go to 240
200   f=w(ngauss)
      return
220   index=3*ngauss
      f=5.5451774*param(index-2)*e(ngauss)*w(ngauss)/param(index)
      return
240   f=5.5451774*param(index-2)*e(ngauss)*w(ngauss)/param(index) _
       *e(ngauss)
      return
#
#  when control reaches this point it is either because 1.- the
#  gaussians are needed, or 2.- derivatives of the base line are
#  needed.
500   if (nwhat-(codeb+3*codeg).lt.0) go to 650
      if (nwhat-(codeb+3*codeg).eq.0) go to 650
      if (nwhat-(codeb+3*codeg).gt.0) go to 900

#  statement 650 computes the base line derivatives.
  650 nderiv=nwhat-3*codeg
#      go to (710,720,730,740,750,760), nderiv
      if (nderiv.eq.1) go to 710
      if (nderiv.eq.2) go to 720
      if (nderiv.eq.3) go to 730
      if (nderiv.eq.4) go to 740
      if (nderiv.eq.5) go to 750
      if (nderiv.eq.6) go to 760
710   f=1.00
      return
720   f=x
      return
730   f=x*x
      return
740   f=x*x*x
      return
750   f=x*x*x*x
      return
760   f=x*x*x*x*x
      return
#  when control comes to statement 900 there are no more deri-
#  vatives to compute, just the functions themselves.
#--if codeg is not zero compute the sum of the codeg gaussians.
#  if codeg is equal to zero skip to the base line section.
900   auxg=0.00
      if (codeg.lt.0) go to 1000
      if (codeg.eq.0) go to 1000
      if (codeg.gt.0) go to 2000
2000  do l=1, codeg {
      index=3*l
      if (abs(w(l)) < 1.0e-30)  {
	auxg = auxg + 1.0e-30
      }
      else
      auxg=auxg+param(index-2)*w(l)
2100 continue
      }
#--if there is no base line return to the calling program.
      if (codeb.lt.0) go to 2200
      if (codeb.eq.0) go to 2200
      if (codeb.gt.0) go to 1000
2200  f=auxg
      return
#--in statement 1000 compute the base line and add the gaussians(
#  if any, from statement 2000.
1000  auxb=0.00
      index=3*codeg + 1
      auxb=param(index)
       if(codeb.lt.2) go to 2300
      do l=2, codeb{
      index=3*codeg+l
      auxb=auxb + param(index)*x**(l-1)
	  }
2300  continue
#--get the total value of the function, namely baseline plus
#  gaussians.
      f=auxb + auxg
      return
      end
