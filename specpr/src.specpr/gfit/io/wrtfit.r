#  6.10.79  dmc  wrtfit  sfortran
#  6.10.80  dmc  fixed call to ptdel - added ipack parameter.  if
#           ipack = 0 (as in this routine) { the spectra and
#           wavelengths will not be packed.  this is so they can be
#           plotted in specpr with the same wavelength files.
#  4.11.80  fixed up verification and other stuff
#  3.15.80: corrected history on data w/ deletions and
#               error file (1st 2 written).
#
#  this routine re-converts the data to specpr units,
#  updates the histories for each output file, and
#  calls fixwr to fill the array and write to disk.
#
#  ---> works only for fitting 1 specpr file <---
#------------------------------------------------------
      subroutine wrtfit(err,b,xmean,sigma,iprot)
	  implicit integer*4 (i-n)
#
	include "../status"
	include "../../common/label1"
#RED
	real*4 bas1        # function
	real f             # function
#
#---  this routine writes the fitted gaussian, the continuum, and
# the result after subtracting the continuum back to the input file
# for specpr to read.

      dimension baslin(256), wavel(256), yy(256), errors(256)
      dimension b(66), err(66)
#---------------------------------------------------------
#  common area which describes 1 specpr data record
#---------------------------------------------------------
#
      character*40 labsp
	  equivalence (labsp,ititl)
      data  delete/-1.23e+34/, fudj/2.7725887/,iy/1hy/
      data ibl/' '/

      call mthwrt(igo,lout,ifilew,iprod)
      if (igo == 110 | igo == 1011) return
      call devlun(4,idr,lin)
      call namdev(lin,itnamr)
      call namdev(lout,itnamw)
      write(6,500)
500   format(1x,'Enter Output File Title...',/,1x,27('-'),'I',/)
      read(5,501) ititlw
501   format(a)

      ipack = 0
      call rdspec(wavel,yy,errors,nwv,ipack)
      nwv = iwvlen

      if (nwv .ne. 256) {
          n1 = nwv + 1
          do j = n1,256 {
              data(j) = delete
          }
      }

      if (invx) {
          do ix = 1,nwv {
              if (wavel(ix)!=delete) wavel(ix) = 1. / wavel(ix)
          }
      }

#---find continuum fit & put into linear space.

      do ix = 1,nwv {
	      xdata = wavel(ix)
		  if (xdata!=delete) {
			  xdata = bas1(xdata,icont,igaus,params)
#-----------------------------------------------
#     inv. log
#-----------------------------------------------
			  if (log) {
				  baslin(ix) = exp(xdata)
			  } else {
				  baslin(ix) = xdata
			  }
		  } else baslin(ix) = delete
      }
#----------------------------------------------
#  write the original data w/ points deleted
#  set = -1.23e+34 for specpr
#------------------------------------------------
#
	  write(ihist,10) itnamr,ifiler

	  labsp=ititlw(1:28)//":data w/del"

      irec = ifilew
      filno = irec
      write(6,200) irec

	  do i = 1,nwv {
          data(i) = yy(i)
      }

      call writsp(lout,irec)
      irec = irec + 1

#-------------------------------------------------
#   write errors if there are any
#--------------------------------------------------
      if (ierror) {

		  write(ihist,15)
		  write(labsp(29:40),105)

          do i = 1,nwv {
              data(i) = errors(i)
          }

          write(6,205) irec
          filno = irec
          call writsp(lout,irec)
          irec = irec + 1

          if (log) {
              do i = 1,nwv {
                  if (baslin(i)!=delete) errors(i) = errors(i) / baslin(i)
				  else errors(i) = 0
              }
          }

      }
#-------------------------------------------------
#   write fitted continuum.
#--------------------------------------------------
      if (icont .ne. 0) {

		  write(ihist,30) icont,itnamw,ifilew

		  write(labsp(29:40),110)

          do i = 1,nwv {
              data(i) = baslin(i)
          }

          write(6,210) irec
          filno = irec
          call writsp(lout,irec)
          irec = irec + 1

      }
#-----------------------------------------------------
#  write individual gaussians
#-----------------------------------------------------
      if (igaus .ne. 0) {
          do ig = 1,igaus {
              index = 3 * ig
              height = params(index-2)
              center = params(index-1)
              width = params(index)

              do ix = 1,nwv {
				  if (wavel(ix)!=delete) {
					  temp = (wavel(ix) - center) / width
					  temp = -( temp * temp)
					  if((temp*fudj) < -85) 
						  t = -85
					  else 
						  t = temp * fudj 
					  tt = t
					  if (t < -70) tt = -70
					  temp = height * exp(tt)

					  if (log) {
						  temp = exp(temp)
					  }

					  data(ix) = temp
				  } else data(ix) = delete
              }

			  write(ihist,90) ig,itnamw,ifilew

			  write(labsp(29:40),160) ig

              write(6,260) irec,ig
              filno = irec
              call writsp(lout,irec)
              irec = irec + 1
          }
      }
#---------------------------------------------
#     calculate fit and write it to disk
#-----------------------------------------------
      nwh = icont + 3 * igaus + 1
      do ix = 1,nwv {
		if(wavel(ix)!=delete) {
          data(ix) = f(nwh,wavel(ix),params,icont,igaus)

          if (log) {
              data(ix) = exp(data(ix))
          }
		} else data(ix) = delete
      }

	  write(ihist,50) igaus,icont,itnamw,ifilew

      if (log) {
		  write(labsp(29:40),129)
      } else {
		  write(labsp(29:40),130)
      }

      write(6,230) irec
      filno = irec
      call writsp(lout,irec)
      irec = irec + 1
#
#---------------------------------------------------
#  remove baseline from fit and write the result
#---------------------------------------------------
      if (icont .ne. 0) {
          if (log) {
              do i = 1,nwv 
      		      if (baslin(i)!=delete) data(i) = data(i) / baslin(i)
				  else data(i) = delete
          } else {
              do i = 1,nwv 
                  if (baslin(i)!=delete) data(i) = data(i) - baslin(i)
				  else data(i) = delete
          }

		  write(ihist,40) igaus,itnamw,ifilew

          if (log) {
			  write(labsp(29:40),119)
          } 
		  else {
			  write(labsp(29:40),120)
          }

          write(6,220) irec
          filno = irec
          call writsp(lout,irec)
          irec = irec + 1
#-------------------------------------------------------
#  write input w/out continuum
#--------------------------------------------------------
          if (log) {
              do i = 1,nwv 
                  if (baslin(i)!=delete) data(i) = yy(i) / baslin(i)
				  else data(i) = delete
          } else {
              do i = 1,nwv 
                  if (baslin(i)!=delete) data(i) = yy(i) - baslin(i)
				  else data(i) = delete
          }

		  write(ihist,45) icont,itnamw,ifilew

         if (log) {
			 write(labsp(29:40),123)
         } else {
			 write(labsp(29:40),124)
         }

         write(6,222) irec
         filno = irec
         call writsp(lout,irec)
         irec = irec + 1
#-------------------------------------------------
#   write errors if there are any
#--------------------------------------------------
         if (ierror) {

			  write(ihist,20) itnamw,ifilew

			  write(labsp(29:40),105)

              do i = 1,nwv {
                  data(i) = errors(i)
              }

              write(6,205) irec
              filno = irec
              call writsp(lout,irec)
              irec = irec + 1
          }
      }
#---------------------------------------------------
#  write residual errors
#---------------------------------------------------
      nwh = icont + 3 * igaus + 1
      do ix = 1,nwv {
          if (wavel(ix)!=delete) data(ix) = f(nwh,wavel(ix),params,icont,igaus)
		  else data(ix) = delete

          if (log) {
              if (data(ix)!=delete) data(ix) = exp(data(ix))
          }
      }

      if (log) {
          do i = 1,nwv 
              if (yy(i)!=delete && data(i)!=delete) data(i) = yy(i) / data(i)
			  else data(i) = delete
      } else {
          do i = 1,nwv 
              if (yy(i)!=delete && data(i)!=delete) data(i) = yy(i) - data(i)
			  else data(i) = delete
      }

      if(log) {
		  write(ihist,59)itnamw,ifilew
      } else {
		  write(ihist,60)itnamw,ifilew
      }

	  write(labsp(29:40),140)

      write(6,240) irec

      filno = irec
      call writsp(lout,irec)
      irec = irec + 1

#---------------------------------------------------------
#   write parameters.
#---------------------------------------------------------

      nparam = 3 * igaus + icont
      ic2 = 2 * igaus

      data(1) = igaus
      data(2) = icont
      iout = 3

      do i = 1,nparam {
          data(iout) = params(i)
          iout = iout + 1
      }

      do i = 1,nparam {
          data(iout) = err(i)
          iout = iout + 1
      }

      if (igaus .gt. 0) {
          do i = 1,ic2 {
              data(iout) = b(i)
              iout = iout + 1
          }
      }

      data(iout) = xmean
      data(iout + 1) = sigma

      iout = iout + 2
      do i=iout,256 {
          data(i) = 0.
      }

	  write(ihist,70) itnamw,ifilew

      write(mhist(1:74),74)

      write(mhist(75:148),75)

      write(mhist(149:222),76)

      if (invx) {
          write(mhist(223:250),77)
      } else {
          write(mhist(223:250),78)
      }

      if (log) {
          write(mhist(251:296),80)
      } else {
          write(mhist(251:296),81)
      }

	  write(labsp(29:40),150)

      write(6,250) irec
      filno = irec
      call writsp(lout,irec)
      irec = irec + 1
      iprot = irec - 1
      iwprot = iprot
      call upprot(lout,iprot)

      return

#--------------------------------------------------------
10    format(4a2,' file ',i4,' used for gaussian fit w/points deleted')
15    format(' errors to previous file')
20    format(60a1)
25    format(12a1)
30    format(' order ',i1,' continuum fit to ',4a2,' file ',i4)
40    format(i2, ' gaussians fit to ',4a2,' file ',i4)
45    format('input w/out order ',i1,' continuum fit to : ',
	 4a2,' file ',i4)
50    format(1x,i1,' gaussians and ',i1,' order cont. fit to: ',4a2,
	 ' file ',i4)
59    format(' residual errors of fit to: ',4a2,' file ',
	 i4,' input / fit')
60    format(' residual errors of fit to: ',4a2,' file ',
	 i4,' input - fit')
70    format(' gfit: parameters for fit to ',4a2,
	 ' file ',i4)
73    format(74a1)
74    format('contents: 1.#gaus, 2.#cont terms, h,c,w for each gaus.,',
	 ' 0,1,2... ord cont.')
75    format('terms., errors for each param., integrated intensity & ',
	 'errors for each')
76    format('gaussian, mean and sigma of residuals') 
77    format(' inverse wavelength and',15x)
78    format(' wavelength and',23x)
79    format(28a1)
85    format(46a1)
80    format('log (base e) space',18x)
81    format('linear space',24x)
90    format(' gaussian ',i1,' fit to ',4a2,' file ',i4)
100   format(':data w/del')
105   format(':errors    ')
110   format(':fit cont  ')
119   format(':fit / cont')
120   format(':fit - cont')
123   format(':inp / cont')
124   format(':inp - cont')
129   format(':fitted g*c')
130   format(':fitted g+c')
140   format(':resid. err')
150   format(':fit param.')
160   format(':gauss no.',i2)
200   format(' file ',i4,' original data')
205   format(' file ',i4,' errors to previous file ')
210   format(' file ',i4,' continuum')
220   format(' file ',i4,' fit with continuum removed')
222   format(' file ',i4,' input with continuum removed')
230   format(' file ',i4,' calculated fit')
240   format(' file ',i4,' residual errors')
250   format(' file ',i4,' parameters')
260   format(' file ',i4,' gaussian # ',i1)
#----------------------------------------------------------
      end
