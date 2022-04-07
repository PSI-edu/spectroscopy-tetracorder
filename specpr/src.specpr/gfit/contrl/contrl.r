#  2.9.80  dmc  contrl  sfortran
#
      subroutine contrl(ndata,nparam,x,y,param,accur,niter,codeb,
	  codeg,a,tryhrd,b,xmean,sigma)
	  implicit integer*4 (i-n)
      logical tryhrd
      integer*2 codeb, codeg
      dimension x(256),y(256),diagnl(66),s(65),a(66,66),b(66),param(66)
#
#   contrl calls the subroutine normal as many times as neces-
#   sary   to achieve a predetermined accuracy accur. it also
#   keeps track of the trend of succesive iterations to make sure
#   the algorithm is converging towards some final solution.
#   contrl also calls the matinv program that actually solves
#   the linearized normal equations. a feature of matinv is
#   that it returns the inverse of the system matrix, which is
#   needed for computing the fitting errors.   the input data,
#   initial guess for the unknown and most of the printed out-
#   put is handled by the calling program.
#
#---start execution of contrl
#
#     write(12,900) ndata,nparam,accur,niter,codeb,codeg
# 900 format(1h1'                            ',
#'summary of information for',
#     ' least squares fit.'/' number of data points...'i4/,
#' free parameters.........'i4/' a tolerance of'f10.6,
#' should be achieved in no more than'i3,' iterations.'/,
#' the function to be fitted is a sum of',i3,
#' parameter base line and'i3,' gaussian(s)')
#---initialization section.
      do l=1, 20
      s(l)=0.00

      iter=1
      nfails=1
      relvar=0.00
#---compute the residuals squared.
      do l=1, ndata {
      xdata=x(l)
      s(1)=s(1) + (y(l) - f(nparam+1,xdata,param,codeb,codeg))**2
	}
#points
      s(1)=sqrt(s(1)/float(ndata-nparam))
      smin=s(1)
#     write(12,1000) s(1), (param(l),l=1, nparam)
1000  format(' the sum of the residuals is ',e12.5,/,
      ' the guess for the parameters was ',3(5e12.5,/))
#---set up the normal equations.
   35 call normal(ndata, nparam,x, y, param, a, b, codeb,codeg)
#---compute the next approximation to the unknown parameters.
      call matinv(nparam, a, b, msignl)
#     if(msignl.ne.-1) write(12,1500) msignl
#1500 format('  msignl is equal to 'i5 //)
#
#   the matrix a(i,j) contains the inverse of the original
#   matrix a. the vector b is the solution of the system,
#   i.e. b is equal to param(this iteration) minus param(previous
#   iteration). consequently the new value of the parameters
#   is given by
      relvar=0.00
      do l=1, nparam  {
      param(l)=param(l) + b(l)
#   and the relative variation by
      relvar=relvar+abs(b(l)/param(l))/float(nparam)
	}
#---calculate the sum of the residuals squared.
      iter=iter + 1
      ntry=iter-1
      do l=1, ndata  {
       xdata=x(l)
       s(iter)=s(iter) + (y(l) - f(nparam+1,xdata,param,codeb,codeg))**2
	}
      s(iter)=sqrt(s(iter)/float(ndata-nparam))
70    continue
#  write(12,1100) ntry,(param(i),i=1, nparam)
#-------------------------------------------------------------------
#  improved printing for terminal
#----------------------------------------------------------------
      iconst = codeg * 3 + 1
      icm1 = iconst - 1
#
      write(6,1103) ntry,(param(k),k=iconst,nparam)
      write(6,1104)
#
      if (codeg .gt. 0) {
      do j = 1,codeg {
      iw1 = j * 3 - 2
      iw2 = iw1 + 1
      iw3 = iw1 + 2
#
      write(6,1105) j, param(iw2), param(iw1), param(iw3)
      }
      }
#
1100  format(/,' status of parameters after iteration number',i3,/,
      ' ',8(e15.7),/,' ',8(e15.7))
1103  format(/' status of parameters after iteration no. ',i3/,
	6x,'const',10x,'x',12x,'x ** 2',7x,'x ** 3',7x,
	'x ** 4',7x,'x ** 5'/,
      1x,6f13.6/)
#     write(12,1300) s(iter), relvar
1104  format(/5x,'gaus #',7x,'center',19x,'height',18x,'width')
1105  format(8x,i2,4x,2(f15.7,10x),f15.7)
#----------------------------------------------------------------
      smin=amin1(smin, s(iter))
      if(s(iter).ge.s(iter-1)) go to 120
1300  format(' the current value of the sum of the residuals is ',
     e12.5,'.  the relative variation of the parameters is'e12.5/)
#   convergence is proceeding normally. if the desired ac-
#   curacy has not been reached yet repeat { the iterating scheme.
      if(ntry.gt.niter) go to 100
      do l=1, nparam
      if(abs(b(l)/param(l)).gt.accur) go to 35

#     write(12,1400) accur,ntry
      write(6,1400) accur,ntry
1400  format(/2x,f15.7,' accuracy achieved in ',i5,' iterations'/)
      go to 110
100   continue
#     write(12,1200) ntry
      write(6,1200) ntry
1200  format(/' program was unable to converge after' i3,' trials.'/)
      go to 110
120   if(tryhrd) go to 125
130   continue
#     write(12,1600) ntry
      write(6,1600) ntry
1600   format(//' iterations are not converging. giving up after',
      i4,' trials.'/)
      go to 110
125   continue
#     write(12,1700)
1700  format(//' try hard option is on, iterating again with a smaller',
      ' step.'/)
#---compute the value of the parameters before the inter-
#   ruption took place. also increase the parameters by 0.20
#   times the computed parameter change.
      do l=1, nparam
      param(l)= param(l) - 0.8*b(l)

#---check whether the residuals have become smaller,
#   unchanged or larger.
      nfails=0
175   snwtry=0.00
      do l=1, ndata  {
      xdata=x(l)
      snwtry=snwtry + (y(l)-f(nparam+1,xdata,param,codeb,codeg))**2
	}
      snwtry=  sqrt(snwtry/float(ndata-nparam))
#---i# the residuals have decreased go back to normal iterat-
#   ing scheme. if they have not decreased make them even
#   closer to the value they had before the interruption.
      if((snwtry - smin) < 0) goto 35
#    nfails counts the number of times the tryhrd option
#    #ailed to work after an interruption.
180   nfails=nfails + 1
      if(nfails.ge.4) go to 200
#     write(12,1800)
      write(6,1800)
1800  format(/' program going through the subiterating scheme.'/)
#---reduce the parameters to closer their previous value
      do l=1, nparam
      param(l)=param(l) - 0.07*b(l)

      go to 175
200   continue
#     write(12,1900)
      write(6,1900)
1900  format(//'   program is completely unable to converge.  control',
     ' returns to main program.'//)
#   when control reaches this point the desired accuracy has
#   been achieved.
#---compute the diagonal terms of the inverse matrix.
110   continue
90    continue
      return
      end
