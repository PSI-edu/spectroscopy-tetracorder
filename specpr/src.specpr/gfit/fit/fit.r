    implicit integer*4 (i-n)	
	include "../status"
#RED
	real f      # function
#common/status/ label(20), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252),idum(12)

	dimension data1(256),  wavel(256), data(256), errors(256)
	dimension x(256), a(66,66),  error(66), b(66), id(3)

	character*40 igfl
	logical tryhrd

	data delete/-1.23e+34/

	call trnsfr(2,error,b,xmean,sigma,igfl)
	call gstart(2,igfl)
	call rstart(2)

	tryhrd=.true.

	ipack = 1
	call rdspec(wavel,data,errors,nwv,ipack)
	write(6,777)nwv
777 format(1x,'Number of channels to work with is ',i4)

	if (invx) {
		do i = 1,nwv {
			wavel(i) = 1. / wavel(i)
		}
	}

	if (log) {
		do i = 1,nwv {
			data(i) = alog(data(i))
		}
	}

	do i = 1,nwv {
		x(i) = wavel(i)
		data1(i) = data(i)
	}

	npoint = nwv

	xsum=0.00
	xsq=0.00
	do l=1, 66 {
		error(l)=0.00
	}

	nparam = 3 * igaus + icont
	ndata = npoint

	write(6,29)
29  format('  working...'/)

	call shuffl(igaus,params)
	call contrl(ndata,nparam,x,data,params,acc,iter,icont,
		igaus,a,tryhrd,b,xmean,sigma)

#---find the residuals
#
	n1 = nparam + 1
#--------------------------------------------------------
#  in counts the number of data points actually fit in
#  contrl.  this is used in calculating the error array
#---------------------------------------------------------
	in = 1
	do l=1, nwv {
		xdata = x(in)
		data(l)=f(n1,xdata,params,icont,igaus)
		x(in) = data1(l) - data(in)
		xsq = xsq + x(in) * x(in)
		xsum = xsum + x(in)
		in = in + 1
	}

	in = in - 1
	npoint = in
	aux = float(npoint)
	xmean = xsum / aux
	sigma=0.
	aux2 = float(npoint)
	if(aux2.gt.0.) sigma=sqrt((xsq-aux*xmean*xmean)/aux2)

	if (icont .ne. 0) {
		ncode=1+3*igaus
		ncode1= ncode+icont
#	write(12,2000) (params(i), i=ncode, ncode1 )
	}
 2000   format(//,' base line results',/,
	' base line offset...',e12.5,' base line slope..',e12.5,
	'  base line non-linear term...',e12.5/,
	' third order term..',
	e12.5,'  fourth order term..'e12.5,'  fifth order term..',
	e12.5////)

	if (igaus .ne. 0) {
		ncode = 3*igaus
#	write(12,2100) (params(l), l=1, ncode)

		do l=1,igaus {
			l1 = 2*l-1
			l2 = l1+1
			j = 3*(l-1)
			b(l1) = params(j+1)*params(j+3)
			b(l2)=a(j+1,j+1)*params(j+3)**2+ _
				(a(j+3,j+1)+a(j+1,j+3)) _
				* params(j+1)*params(j+3)+ _
				a(j+3,j+3)*params(j+1)**2
			b(l2)=sigma*sqrt(abs(b(l2)))
		}
	}

2100    format(' gaussian parameters'/,
      ' amplitude.....'e12.5,' center.....'e12.5,' width.....'e12.5/,
      ' amplitude.....'e12.5,' center.....'e12.5,' width.....'e12.5/,
      ' amplitude.....'e12.5,' center.....'e12.5,' width.....'e12.5/,
      ' amplitude.....'e12.5,' center.....'e12.5,' width.....'e12.5/,
      ' amplitude.....'e12.5,' center.....'e12.5,' width.....'e12.5/   )
#
#---compute the error for each parameter.
#
	do l=1, nparam {
		error(l) = sigma * sqrt(abs(a(l,l)))
	}

#   write(12,2350)
	write(6,2350)
2350    format(/' the fitting errors are respectively : '/,
		6x,'const',10x,'x',10x,'x ** 2',7x 'x ** 3',7x,
		'x ** 4',7x,'x ** 5'/)

	if (icont .ne. 0) {
		naux=3*igaus + 1
#   	write(12,2400) (error(l), l=naux, nparam)
		write(6,2400) (error(l), l=naux, nparam)
	}
2400    format(1x,6e12.2)

	if (igaus .ne. 0) {
		write(6,2449)
#	write(12,2449)
2449            format(/5x,'gaus #',7x,'center',19x,'height',18x,'width')

		do j = 1,igaus {
			j1 = 3 * j - 2
			j2 = j1 + 1
			j3 = j1 + 2
			write(6,2450) j,error(j2),error(j1),error(j3)
#		write(12,2450) j,error(j2),error(j1),error(j3)
		}

2450            format(8x,i2,4x,2(e15.7,10x),e15.7)

		naux=2*igaus
#	write(12,5001) (b(l),l=1,naux)
5001            format(/' integrated line intensities'/,
			(' ampl''width.....',e12.5,' error.....',e12.5/))
	}

#   write(12,2200) xmean, sigma
	write(6,2200) xmean, sigma
2200    format(/' the mean value of the residuals is'e12.5/,
		' with a dispersion of          'e12.5//)
#
#     call wrtfit(x,yy,npoint,iadd,error,b,xmean,sigma)
#
#
949     format(' output file : ',4a2,'  file ',i4//)
#
#    write(12,5000)
5000    format(26x,'continued from previous run')
#-----------------------------------------------------
	call shuffl(igaus,params)
	call trnsfr(1,error,b,xmean,sigma,igfl)
	call gstart(1,igfl)
	end
