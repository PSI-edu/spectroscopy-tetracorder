      subroutine chgfit
	  implicit integer*4 (i-n)
#
#     common/status/ label(20), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(20), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252), idum(12)
#
      include "../status"
      character iyes*3,ino*2
#
#
#     data iyes/'YES'/, ino/'NO '/
      iyes = 'yes'
	  ino  = 'no'
#
      write(6,3)
3     format(/,1x,'ESTIMATE PARAMETER REVIEW',//)
      write(6,10) label
10    format(1x,'a. ',a,'<*')
      write(6,20) idr,ifiler,ititlr
20    format(1x,'b. Fit: ',a1,i4,2x,' file: ',a)
      write(6,30) iwavel,iwvlen
30    format(1x,'w. Wavelength File: ',i2,5x,i4,' Channels')
      write(6,40) igaus
40    format(1x,'d. Number of Gaussians -> ',i2)
      write(6,50) icont
50    format(1x,'c. Number of Continuums -> ',i2)
      write(6,60) iter
60    format(1x,'f. Number of Iterations -> ',i3)
      write(6,70) acc
70    format(1x,'g. Accuracy -> ',f15.7)
      if (ierror) {
	write(6,80) iyes
%80      format(1x,'h. Errors? ',a)
      } else {
	write(6,80) ino
      }
      if (invx) {
	write(6,90) iyes
%90      format(1x,'i. Invert Wavelengths? ',a)
      } else {
	write(6,90) ino
      }
      if (log) {
	write(6,100) iyes
%100     format(1x,'j. Log Space? ',a)
      } else {
	write(6,100) ino
      }
      write(6,110)
110   format(1x,'k. Deleted Points:')
      write(6,200)(iptdel(1,ii),ii=1,10)
      write(6,200)(iptdel(2,ii),ii=1,10)
%200   format(1x,10(i5))
      return
      end
