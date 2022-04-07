      subroutine normal(ndata, nparam, x, y, param, a, b, codeb,codeg)
	  implicit integer*4 (i-n)
      dimension x(256),y(256),a(66,66),b(66),aux(66),param(66)
      integer*2 codeb, codeg
#RED
	real f      # function
#---normal sets up the normal equations for a least squares  fit.
#
#-----------------------------------------------------------------------
#                                                                      i
#   normal computes the matrix elements of the system of linear-       i
#   ized normal equations (kaper et al. 1966 ban 18, 465).             i
#                                                                      i
#   the function to be fitted is called from a function statement      i
#   f(nwhat, x, param, code) where nwhat=1, 2,...represents the        i
#   first, second, etc. derivative of the function respect to the      i
#   sought parameters and nwhat=nparam+1 is the function itself.       i
#   code caracterizes the kind of function, for example a line, three  i
#   gaussians, etc.                                                    i
#                                                                      i
#  ndata     number of data points, i.e. number of (x,y) pairs,        i
#                                                                      i
#  nparam    number of unknown parameters,                             i
#                                                                      i
#  x,y       the input data arrays. x and y values are transfer-       i
#            red as two arrays. therefore, the calling program         i
#            should see that they are correctly paired up,             i
#                                                                      i
#  param     an array representing the nparam unknown parameters,      i
#                                                                      i
#  a         the matrix of the system, i.e. a  matrix of dimension     i
#            nparam x nparam,                                          i
#                                                                      i
#  b         the inhomogeneous term of the system.                     i
#                                                                      i
#-----------------------------------------------------------------------
#
#------setting up the normal equations.
#
#   it is not necessary to compute all the elements of the matrix
#   because it is a symmetrical matrix.
#---clear the matrix and vector space.
      do  i=1, 66 {
      b(i)=0.00
      do j=1, 66{
      a(i,j)=0.00
    5 continue  }}
#---compute the matrix elements.
      do  k=1, ndata {
      xdata=x(k)
      do  i=1, nparam{
      aux(i)=f(i, xdata, param, codeb, codeg)
   20 continue }
#---calculate diagonal and above diagonal terms, also the non-
#   homogeneous term.
      do  i=1, nparam{
      b(i)=b(i)+(y(k)-f(nparam+1,xdata,param,codeb,codeg))*aux(i)
      do  j=i, nparam{
      a(i,j)=a(i,j) + aux(i)*aux(j)
   10 continue }}}
#---find the remaining off-diagonal terms.
      do i=1, nparam {
      do  j=i, nparam{
      a(j,i)=a(i,j)
   30 continue        }}
      return
      end
