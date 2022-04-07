subroutine fft(data,nn,isign,iform,work)
implicit integer*4 (i-n)
#
#
#     Feb 12, 1987: converted to RATFOR by Roger N. Clark and made
#                   additions of implicit integer, and set
#                   dimensions of arrays for specpr.
#                   ndim and iform removed from call line and fixed.
#
#     old form of subroutine:
#            subroutine fourt(data,nn,ndim,isign,iform,work)
#     where:
#            data = data array
#            nn(i) = dimension sizes (set to 1 for specpr)
#            ndim  = number of dimensions
#            isign =  1 for transform
#                  = -1 for inverse transform
#            iform = 1 if data complex
#                  = 0 if data real
#            work  = working storage: up to 2*max(nn(i)) if not
#                    a power of 2.
#
#     the cooley-tukey fast fourier transform in usasi basic fortran
#
#     transform(j1,j2,,,,) = sum(data(i1,i2,,,,)*w1**((i2-1)*(j2-1))
#                                 *w2**((i2-1)*(j2-1))*,,,),
#     where i1 and j1 run from 1 to nn(1) and w1=exp(isign*2*pi=
#     sqrt(-1)/nn(1)), etc.  there is no limit on the dimensionality
#     (number of subscripts) of the data array.  if an inverse
#     transform (isign=+1) is performed upon an array of transformed
#     (isign=-1) data, the original data will reappear.
#     multiplied by nn(1)*nn(2)*,,,  the array of input data must be
#     in complex format.  however, if all imaginary parts are zero (i.e.
#     the data are disguised real) running time is cut up to forty per-
#     cent.  (for fastest transform of real data, nn(1) should be even.)
#     the transform values are always complex and are returned in the
#     original array of data, replacing the input data.  the length
#     of each dimension of the data array may be any integer.  the
#     program runs faster on composite integers than on primes, and is
#     particularly fast on numbers rich in factors of two.
#
#     timing is in fact given by the following formula.  let ntot be the
#     total number of points (real or complex) in the data array, that
#     is, ntot=nn(1)*nn(2)*...  decompose ntot into its prime factors,
#     such as 2**k2 * 3**k3 * 5**k5 * ...  let sum2 be the sum of all
#     the factors of two in ntot, that is, sum2 = 2*k2.  let sumf be
#     the sum of all other factors of ntot, that is, sumf = 3*k3*5*k5*..
#     the time taken by a multidimensional transform on these ntot data
#     is t = t0 + ntot*(t1+t2*sum2+t3*sumf).  on the cdc 3300 (floating
#     point add time = six microseconds), t = 3000 + ntot*(600+40*sum2+
#     175*sumf) microseconds on complex data.
#
#     implementation of the definition by summation will run in a time
#     proportional to ntot*(nn(1)+nn(2)+...).  for highly composite ntot
#     the savings offered by this program can be dramatic.  a one-dimen-
#     sional array 4000 in length will be transformed in 4000*(600+
#     40*(2+2+2+2+2)+175*(5+5+5)) = 14.5 seconds versus about 4000*
#     4000*175 = 2800 seconds for the straightforward technique.
#
#     the fast fourier transform places three restrictions upon the
#     data.
#     1.  the number of input data and the number of transform values
#     must be the same.
#     2.  both the input data and the transform values must represent
#     equispaced points in their respective domains of time and
#     frequency.  calling these spacings deltat and deltaf, it must be
#     true that deltaf=2*pi/(nn(i)*deltat).  of course, deltat need not
#     be the same for every dimension.
#     3.  conceptually at least, the input data and the transform output
#     represent single cycles of periodic functions.
#
#     the calling sequence is--
#     call fourt(data,nn,ndim,isign,iform,work)
#
#     data is the array used to hold the real and imaginary parts
#     of the data on input and the transform values on output.  it
#     is a multidimensional floating point array, with the real and
#     imaginary parts of a datum stored immediately adjacent in storage
#     (such as fortran iv places them).  normal fortran ordering is
#     expected, the first subscript changing fastest.  the dimensions
#     are given in the integer array nn, of length ndim.  isign is -1
#     to indicate a forward transform (exponential sign is -) and +1
#     for an inverse transform (sign is +).  iform is +1 if the data are
#     complex, 0 if the data are real.  if it is 0, the imaginary
#     parts of the data must be set to zero.  as explained above, the
#     transform values are always complex and are stored in array data.
#     work is an array used for working storage.  it is floating point
#     real, one dimensional of length equal to twice the largest array
#     dimension nn(i) that is not a power of two.  if all nn(i) are
#     powers of two, it is not needed and may be replaced by zero in the
#     calling sequence.  thus, for a one-dimensional array, nn(1) odd,
#     work occupies as many storage locations as data.  if supplied,
#     work must not be the same array as data.  all subscripts of all
#     arrays begin at one.
#
#     example 1.  three-dimensional forward fourier transform of a
#     complex array dimensioned 32 by 25 by 13 in fortran iv.
#     dimension data(32,25,13),work(50),nn(3)
#     complex data
#     data nn/32,25,13/
#     do 1 i=1,32
#     do 1 j=1,25
#     do 1 k=1,13
#  1  data(i,j,k)=complex value
#     call fourt(data,nn,3,-1,1,work)
#     example 2.  one-dimensional forward transform of a real array of
#     length 64 in fortran ii,
#     dimension data(2,64)
#     do 2 i=1,64
#     data(1,i)=real part
#  2  data(2,i)=0.
#     call fourt(data,64,1,-1,0,0)
#
#     there are no error messages or error halts in this program.  the
#     program returns immediately if ndim or any nn(i) is less than one.
#
#     program by norman brenner from the basic program by charles
#     rader,  june 1967.  the idea for the digit reversal was
#     suggested by ralph alter.
#
#     this is the fastest and most versatile version of the fft known
#     to the author.  a program called four2 is available that also
#     performs the fast fourier transform and is written in usasi basic
#     fortran.  it is about one third as long and restricts the
#     dimensions of the input array (which must be complex) to be powers
#     of two.  another program, called four1, is one tenth as long and
#     runs two thirds as fast on a one-dimensional complex array whose
#     length is a power of two.
#
#     reference--
#     ieee audio transactions (june 1967), special issue on the fft.
#
#was: dimension data(7926),nn(1),ifact(32),work(14592)

real*4 data(7926), work(14592)
integer*4 nn(1),ifact(32)

data twopi/6.2831853071796/,rthlf/0.70710678118655/

#   the following two lines are fixed for specpr
ndim = 1

#RED Initialize np0 to 1, nprev to 0, and twowr to 0
	np0 = 1
	nprev = 0
	twowr = 0

if (ndim>=1) {
	ntot = 2
	do idim = 1,ndim {
		if (nn(idim)<=0)
			return
		ntot = ntot*nn(idim)
	} # end do idim =1,ndim
#
#     main loop for each dimension
#
	np1 = 2
	do idim = 1,ndim {
		n = nn(idim)
		np2 = np1*n
		if (n < 1) {
			break 1
		}
		if (n != 1) {
#
#     is n a power of two and if not, what are its factors
#
			m = n
			ntwo = np1
			ifx = 1
			idiv = 2
			repeat {
				iquot = m/idiv
				irem = m-idiv*iquot
				if (iquot<idiv) {
					go to 10
				}
				if (irem!=0) {
					break 1
				}
				ntwo = ntwo+ntwo
				ifact(ifx) = idiv
				ifx = ifx+1
				m = iquot
			}
			idiv = 3
			inon2 = ifx
			repeat {
				iquot = m/idiv
				irem = m-idiv*iquot
				if (iquot<idiv) {
					break 1
				}
				if (irem!=0) {
					idiv = idiv+2
				}
				else {
					ifact(ifx) = idiv
					ifx = ifx+1
					m = iquot
				} # end if irem1=0 else
			} # end do iquot = m/idiv
			go to 20
10                      inon2 = ifx
			if (irem==0) {
				ntwo = ntwo+ntwo
				go to 30
			} # end if irem==0
20			ifact(ifx) = m
#
#     separate four cases--
#        1. complex transform or real transform for the 4th, 9th,etc.
#           dimensions.
#        2. real transform for the 2nd or 3rd dimension.  method--
#           transform half the data, supplying the other half by con-
#           jugate symmetry.
#        3. real transform for the 1st dimension, n odd.  method--
#           set the imaginary parts to zero.
#        4. real transform for the 1st dimension, n even.  method--
#           transform a complex array of length n/2 whose real parts
#           are the even numbered real values and whose imaginary parts
#           are the odd numbered real values.  separate and supply
#           the second half by conjugate symmetry.
#
30                      icase = 1
			ifmin = 1
			i1rng = np1
			if (idim<4) {
				if (iform<=0) {
					icase = 2
					i1rng = np0*(1+nprev/2)
					if (idim<=1) {
						icase = 3
						i1rng = np1
						if (ntwo>np1) {
							icase = 4
							ifmin = 2
							ntwo = ntwo/2
							n = n/2
							np2 = np2/2
							ntot = ntot/2
							i = 1
							do j = 1,ntot {
								data(j) = data(i)
								i = i+2
							} # end do j=1,ntot
						} # end if ntwo>np1
					} # end if idim<=1
				} # end if iform<=0
			} # end if idim<4
#
#     shuffle data by bit reversal, since n=2**k.  as the shuffling
#     can be done by simple interchange, no working array is needed
#
			if (ntwo<np2) {
#
#     shuffle data by digit reversal for general n
#
				nwork = 2*n
				do i1 = 1,np1,2 {
					do i3 = i1,ntot,np2 {
						j = i3
						do i = 1,nwork,2 {
							if (icase==3) {
								work(i) = data(j)
								work(i+1) = 0.
							}
							else {
								work(i) = data(j)
								work(i+1) = data(j+1)
							} # end if icase==3 else
							ifp2 = np2
							ifx = ifmin
900       						continue
								ifp1 = ifp2/ifact(ifx)
								j = j+ifp1
								if (j-i3<ifp2) {
									break 1
								}
								j = j-ifp2
								ifp2 = ifp1
								ifx = ifx+1
							if (.not. ifp2<=np1) go to 900
						} # end do i=1,nwork,2
						i2max = i3+np2-np1
						i = 1
						do i2 = i3,i2max,np1 {
							data(i2) = work(i)
							data(i2+1) = work(i+1)
							i = i+2
						} # end do i2=i3+hp2-np1
					} # end do i3=i1,ntot,np2
				} # end do i1=1,hpl,2
			} # end if ntwo<np2
			else {
				np2hf = np2/2
				j = 1
				do i2 = 1,np2,np1 {
					if (j<i2) {
						i1max = i2+np1-2
						do i1 = i2,i1max,2 {
							do i3 = i1,ntot,np2 {
								j3 = j+i3-i2
								tempr = data(i3)
								tempi = data(i3+1)
								data(i3) = data(j3)
								data(i3+1) = data(j3+1)
								data(j3) = tempr
								data(j3+1) = tempi
							} # end do i3=i1,ntot,np2
						} # end do i1=i2,i1max,2
					}
					m = np2hf
					while (j>m) {
						j = j-m
						m = m/2
						if (m<np1) {
							break 1
						}
					} # end while j>m
					j = j+m
				} # end do i2=1,np2,np1
			} # end if ntwo<np2 else
#
#     main loop for factors of two.  perform fourier transforms of
#     length four, with one of length two if needed.  the twiddle factor
#     w=exp(isign*2*pi*sqrt(-1)*m/(4*mmax)).  check for w=isign*sqrt(-1)
#     and repeat for w=w*(1+isign*sqrt(-1))/sqrt(2).
#
			if (ntwo>np1) {
				np1tw = np1+np1
				ipar = ntwo/np1
				repeat {
					if (ipar<2) {
						go to 40
					}
					if (ipar==2) {
						break 1
					}
					ipar = ipar/4
				} # end do
				do i1 = 1,i1rng,2 {
					do k1 = i1,ntot,np1tw {
						k2 = k1+np1
						tempr = data(k2)
						tempi = data(k2+1)
						data(k2) = data(k1)-tempr
						data(k2+1) = data(k1+1)-tempi
						data(k1) = data(k1)+tempr
						data(k1+1) = data(k1+1)+tempi
					} # end do k1=i1,ntot,np1tw
				} # end do i1=1,i1rng,2
40                              mmax = np1
				while (mmax<ntwo/2) {
					lmax = max0(np1tw,mmax/2)
					do l = np1,lmax,np1tw {
						m = l
						if (mmax<=np1) {
							go to 50
						}
						theta = -twopi*float(l)/float(4*mmax)
						if (isign>=0) {
							theta = -theta
						}
						wr = cos(theta)
						wi = sin(theta)
# RED 
						repeat {
							w2r = wr*wr-wi*wi
							w2i = 2.*wr*wi
							w3r = w2r*wr-w2i*wi
							w3i = w2r*wi+w2i*wr
50                                                      continue
						        do i1 = 1,i1rng,2 {
								kmin = i1+ipar*m
								if (mmax<=np1) {
									kmin = i1
								}
								kdif = ipar*mmax
								repeat {
									kstep = 4*kdif
									if (kstep>ntwo) {
										break 1
									}
									do k1 = kmin,ntot,kstep {
										k2 = k1+kdif
										k3 = k2+kdif
										k4 = k3+kdif
										if (mmax<=np1) {
											u1r = data(k1)+data(k2)
											u1i = data(k1+1)+data(k2+1)
											u2r = data(k3)+data(k4)
											u2i = data(k3+1)+data(k4+1)
											u3r = data(k1)-data(k2)
											u3i = data(k1+1)-data(k2+1)
											if (isign<0) {
												u4r = data(k3+1)-data(k4+1)
												u4i = data(k4)-data(k3)
											}
											else {
												u4r = data(k4+1)-data(k3+1)
												u4i = data(k3)-data(k4)
											} # end if isign<0 else
										} # end if mmax<=np1
										else {
											t2r = w2r*data(k2)-w2i*data(k2+1)
											t2i = w2r*data(k2+1)+w2i*data(k2)
											t3r = wr*data(k3)-wi*data(k3+1)
											t3i = wr*data(k3+1)+wi*data(k3)
											t4r = w3r*data(k4)-w3i*data(k4+1)
											t4i = w3r*data(k4+1)+w3i*data(k4)
											u1r = data(k1)+t2r
											u1i = data(k1+1)+t2i
											u2r = t3r+t4r
											u2i = t3i+t4i
											u3r = data(k1)-t2r
											u3i = data(k1+1)-t2i
											if (isign<0) {
												u4r = t3i-t4i
												u4i = t4r-t3r
											}
											else {
												u4r = t4i-t3i
												u4i = t3r-t4r
											} # end if isign<0 else
										} # end if mmax<=np1 else
										data(k1) = u1r+u2r
										data(k1+1) = u1i+u2i
										data(k2) = u3r+u4r
										data(k2+1) = u3i+u4i
										data(k3) = u1r-u2r
										data(k3+1) = u1i-u2i
										data(k4) = u3r-u4r
										data(k4+1) = u3i-u4i
									} # end do k1=kmin,ntot,kstep
									kdif = kstep
									kmin = 4*(kmin-i1)+i1
								} # end do
							} # end do i1=1,i1rng,2
							m = m+lmax
							if (m>mmax) {
								break 1
							}
							if (isign<0) {
								tempr = wr
								wr = (wr+wi)*rthlf
								wi = (wi-tempr)*rthlf
							}
							else {
								tempr = wr
								wr = (wr-wi)*rthlf
								wi = (tempr+wi)*rthlf
							} # end if isign<0 else
						} # end do
					} # end do l=np1,lmax,np1tw
					ipar = 3-ipar
					mmax = mmax+mmax
				} # end while mmax<ntwo/2
			} # end if ntwo>npl
#
#     main loop for factors not equal to two.  apply the twiddle factor
#     w=exp(isign*2*pi*sqrt(-1)*(j1-1)*(j2-j1)/(ifp1+ifp2)), then
#     perform a fourier transform of length ifact(ifx), making use of
#     conjugate symmetries.
#
			if (ntwo < np2) {
				ifp1 = ntwo
				ifx = inon2
				np1hf = np1/2
920       			continue
					ifp2 = ifact(ifx)*ifp1
					j1min = np1+1
					if (j1min<=ifp1) {
						do j1 = j1min,ifp1,np1 {
							theta = -twopi*float(j1-1)/float(ifp2)
							if (isign>=0) {
								theta = -theta
							}
							wstpr = cos(theta)
							wstpi = sin(theta)
							wr = wstpr
							wi = wstpi
							j2min = j1+ifp1
							j2max = j1+ifp2-ifp1
							do j2 = j2min,j2max,ifp1 {
								i1max = j2+i1rng-2
								do i1 = j2,i1max,2 {
									do j3 = i1,ntot,ifp2 {
										tempr = data(j3)
										data(j3) = data(j3)*wr-data(j3+1)*wi
										data(j3+1) = tempr*wi+data(j3+1)*wr
									} # end do j3=i1,ntot,ifp2
								} # end do i1=j2,i1max,2
								tempr = wr
								wr = wr*wstpr-wi*wstpi
								wi = tempr*wstpi+wi*wstpr
							} # end do j2=j2min,j2max,ifp1
						} # end do j1=j1min,ifp1,np1
					} # end if j1min<=ifp1
					theta = -twopi/float(ifact(ifx))
					if (isign>=0) {
						theta = -theta
					}
					wstpr = cos(theta)
					wstpi = sin(theta)
					j2rng = ifp1*(1+ifact(ifx)/2)
					do i1 = 1,i1rng,2 {
						do i3 = i1,ntot,np2 {
							j2max = i3+j2rng-ifp1
							do j2 = i3,j2max,ifp1 {
								j1max = j2+ifp1-np1
								do j1 = j2,j1max,np1 {
									j3max = j1+np2-ifp2
									do j3 = j1,j3max,ifp2 {
										jmin = j3-j2+i3
										jmax = jmin+ifp2-ifp1
										i = 1+(j3-i3)/np1hf
										if (j2<=i3) {
											sumr = 0.
											sumi = 0.
											do j = jmin,jmax,ifp1 {
												sumr = sumr+data(j)
												sumi = sumi+data(j+1)
											}  
											work(i) = sumr
											work(i+1) = sumi
										} # end if j2<=i3
										else {
											iconj = 1+(ifp2-2*j2+i3+j3)/np1hf
											j = jmax
											sumr = data(j)
											sumi = data(j+1)
											oldsr = 0.
											oldsi = 0.
											j = j-ifp1
910        										continue
												tempr = sumr
												tempi = sumi
												sumr = twowr*sumr-oldsr+data(j)
												sumi = twowr*sumi-oldsi+data(j+1)
												oldsr = tempr
												oldsi = tempi
												j = j-ifp1
											if (.not. j<=jmin) go to 910
											tempr = wr*sumr-oldsr+data(j)
											tempi = wi*sumi
											work(i) = tempr-tempi
											work(iconj) = tempr+tempi
											tempr = wr*sumi-oldsi+data(j+1)
											tempi = wi*sumr
											work(i+1) = tempr+tempi
											work(iconj+1) = tempr-tempi
										} # end if j2<=i3 else
									} # end do j3=j1,j3max,ifp2
								} # end do j1=j2,j1max,np1
								if (j2<=i3) {
									wr = wstpr
									wi = wstpi
								}
								else {
									tempr = wr
									wr = wr*wstpr-wi*wstpi
									wi = tempr*wstpi+wi*wstpr
								} # end if j2<=i3 else
								twowr = wr+wr
							} # end do j2=i3,j2max,ifp1
							i = 1
							i2max = i3+np2-np1
							do i2 = i3,i2max,np1 {
								data(i2) = work(i)
								data(i2+1) = work(i+1)
								i = i+2
							} # end do i2=i3,i2max,np1
						} # end do i3=i1,ntot,np2
					} # end do i1=1,i1rng,2
					ifx = ifx+1
					ifp1 = ifp2
				if (.not. ifp1>=np2) go to 920
			} # end if ntwo<np2
#
#     complete a real transform in the 1st dimension, n even, by con-
#     jugate symmetries.
#     RED  replaced switch set with if
#
			if(icase==2) {
#
#     complete a real transform for the 2nd or 3rd dimension by
#     conjugate symmetries.
#
				if (i1rng<np1) {
					do i3 = 1,ntot,np2 {
						i2max = i3+np2-np1
						do i2 = i3,i2max,np1 {
							imin = i2+i1rng
							imax = i2+np1-2
							jmax = 2*i3+np1-imin
							if (i2>i3) {
								jmax = jmax+np2
							}
							if (idim>2) {
								j = jmax+np0
								do i = imin,imax,2 {
									data(i) = data(j)
									data(i+1) = -data(j+1)
									j = j-2
								} # end do i=imin,imax,2
							} # end if idim>2
							j = jmax
							do i = imin,imax,np0 {
								data(i) = data(j)
								data(i+1) = -data(j+1)
								j = j-np0
							} # end do i=imin,imax,np0
						} # end do i2=i3,i2max,np1
					} # end do i3=1,ntot,np2
				} # end if i1rng<np1
			} # end if icase==2
			else {
				if(icase==4) {
					nhalf = n
					n = n+n
					theta = -twopi/float(n)
					if (isign>=0) {
						theta = -theta
					}
					wstpr = cos(theta)
					wstpi = sin(theta)
					wr = wstpr
					wi = wstpi
					imin = 3
					jmin = 2*nhalf-1
					while (imin<jmin) {
						j = jmin
						do i = imin,ntot,np2 {
							sumr = (data(i)+data(j))/2.
							sumi = (data(i+1)+data(j+1))/2.
							difr = (data(i)-data(j))/2.
							difi = (data(i+1)-data(j+1))/2.
							tempr = wr*sumi+wi*difr
							tempi = wi*sumi-wr*difr
							data(i) = sumr+tempr
							data(i+1) = difi+tempi
							data(j) = sumr-tempr
							data(j+1) = -difi+tempi
							j = j+np2
						} # end do i=imin,ntot,np2
						imin = imin+2
						jmin = jmin-2
						tempr = wr
						wr = wr*wstpr-wi*wstpi
						wi = tempr*wstpi+wi*wstpr
				 	} # end while imin<jmin
					if (imin==jmin && isign<0) {
							do i = imin,ntot,np2 {
								data(i+1) = -data(i+1)
							} # end do
					} # end if imin==jmin && isign<0
					np2 = np2+np2
					ntot = ntot+ntot
					j = ntot+1
					imax = ntot/2+1
					repeat {
						imin = imax-2*nhalf
						i = imin
						repeat {
							i = i+2
							j = j-2
							if (i>=imax) {
								break 1
							}
							data(j) = data(i)
							data(j+1) = -data(i+1)
						} # end do
						data(j) = data(imin)-data(imin+1)
						data(j+1) = 0.
						if (i>=j) {
							break 1
						}
						repeat {
							i = i-2
							j = j-2
							if (i<=imin) {
								break 1
							}
							data(j) = data(i)
							data(j+1) = data(i+1)
						} # end do
						data(j) = data(imin)+data(imin+1)
						data(j+1) = 0.
						imax = imin
					} # end do
					data(1) = data(1)+data(2)
					data(2) = 0.
				} # end if icase==4
			} # end if icase==2 else
# RED end if cases
#   recheck all }
		} # end if n!=1
#
#     end of loop on each dimension
#
		np0 = np1
		np1 = np2
		nprev = n
	} # end do idim=1,ndim
} # end if ndim>=1
return
end



