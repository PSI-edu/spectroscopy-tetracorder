	program btest
        implicit integer*4 (i-n)
c
c test speed of bandmap algorithm
c
	integer*4 SIZE
	parameter (SIZE = 24)

        integer*4 ttyout
        integer*4 cl1,cl2,cr1,cr2,ier,minch,maxch,ifeattype

	common /a1/ wav, rflib, rfobs, rflibc, rfobsc

        real*4 wav(SIZE), rflib(SIZE), rfobs(SIZE)
        real*4 rflibc(SIZE), rfobsc(SIZE), rftemp(SIZE)

        ttyout = 6
c wav:
	wav(1) = 2.260099
	wav(2) = 2.267506
	wav(3) = 2.274911
	wav(4) = 2.282315
	wav(5) = 2.289719
	wav(6) = 2.297122
	wav(7) = 2.304523
	wav(8) = 2.311924
	wav(9) = 2.319323
	wav(10)= 2.326722
	wav(11)= 2.334119
	wav(12)= 2.341516
	wav(13)= 2.348911
	wav(14)= 2.356306
	wav(15)= 2.363699
	wav(16)= 2.371092
	wav(17)= 2.378484
	wav(18)= 2.385874
	wav(19)= 2.393264
	wav(20)= 2.400653
	wav(21)= 2.408040
	wav(22)= 2.415427
	wav(23)= 2.422813
	wav(24)= 2.430197

c obs:
	rfobs(1) = 0.960000
	rfobs(2) = 0.958725
	rfobs(3) = 0.952158
	rfobs(4) = 0.947734
	rfobs(5) = 0.947457
	rfobs(6) = 0.944540
	rfobs(7) = 0.940309
	rfobs(8) = 0.946524
	rfobs(9) = 0.936050
	rfobs(10)= 0.932932
	rfobs(11)= 0.933701
	rfobs(12)= 0.936422
	rfobs(13)= 0.941310
	rfobs(14)= 0.932678
	rfobs(15)= 0.939739
	rfobs(16)= 0.937708
	rfobs(17)= 0.942546
	rfobs(18)= 0.940985
	rfobs(19)= 0.942890
	rfobs(20)= 0.946657
	rfobs(21)= 0.946016
	rfobs(22)= 0.943029
	rfobs(23)= 0.948675
	rfobs(24)= 0.954809

c library:
	rflib(1) = 0.506496
	rflib(2) = 0.506888
	rflib(3) = 0.506582
	rflib(4) = 0.503448
	rflib(5) = 0.500118
	rflib(6) = 0.499337
	rflib(7) = 0.499804
	rflib(8) = 0.496011
	rflib(9) = 0.488965
	rflib(10)= 0.478441
	rflib(11)= 0.473013
	rflib(12)= 0.474604
	rflib(13)= 0.475324
	rflib(14)= 0.474931
	rflib(15)= 0.478359
	rflib(16)= 0.484820
	rflib(17)= 0.485823
	rflib(18)= 0.482743
	rflib(19)= 0.483050
	rflib(20)= 0.486246
	rflib(21)= 0.483204
	rflib(22)= 0.481595
	rflib(23)= 0.487014
	rflib(24)= 0.485523


c bdmset: setup:
ccc  parameter description:
ccc     INPUT:
ccc        rflib  = reference library spectrum  R*4 (cr2 elements)
ccc        cl1    = continuum point begin on left side of band  I*4
ccc        cl2    = continuum point end on left side of band  I*4
ccc                   note: cl2 >= cl1  (checked)
ccc        cr1    = continuum point begin on right
ccc                 note: cr1 > cl2 + 1 (checked)
ccc        cr2    = continuum point end on right side of band  I*4
ccc                   note: cr2 >= cr1 (checked)
ccc                   note: cr2 also determines the max array sizes
ccc     OUTPUT:
ccc        rflibc = reference library spectrum, continuum
ccc                                 removed  R*4 (cr2 elements)
ccc        minch  = minimum channel in the reference library spectrum.
ccc                 This is defined to be the band minimum.
ccc        maxch  = maximum channel in the referenc
ccc                 This is defined to be the band maximum.
ccc        ifeattype = -1 is an emission feature
ccc                  =  1 is an absorption band
ccc        ier    = error detected in input:
ccc                    = 0 no error
ccc                    = 1 error
ccc
	cl1 = 1
	cl2 = 3
	cr1 = 23
	cr2 = 24
	

        call bdmset (wav,rflib,cl1,cl2,cr1,cr2,
     c                     rflibc,minch,maxch,ifeattype,ier)

	if (ier .ne. 0) write (ttyout,*) 'bdmset ERROR:', ier



	do 1000 i = 1, 10000000

ccc  parameter description:
ccc     INPUT:
ccc        wav    = wavelengths  R*4  (cr2 elements)
ccc        rflibc = reference library spectrum, continuum
ccc                                 removed  R*4 (cr2 elements)
ccc        rfobs  = observed spectrum  R*4 (cr2 elements)
ccc        cl1    = continuum point begin on left side of band  I*4
ccc        cl2    = continuum point end on left side of band  I*4
ccc                   note: cl2 >= cl1  (not checked)
ccc        cr1    = continuum point begin on right side of band I*4
ccc                   note: cr1 > cl2 + 1 (not checked)
ccc        cr2    = continuum point end on right side of band  I*4
ccc                   note: cr2 >= cr1 (not checked)
ccc                   note: cr2 also determines the max array sizes
ccc        ictrol = error message control flag:
ccc                   = 0 don't print error messages, just set
ccc                       output to deleted values.
ccc                   = 1 print error messages and set output to
ccc                       deleted values.
ccc        minch  = minimum channel in the library spectrum.
ccc        maxch  = maximum channel in the reference library spectrum.
ccc                 This is defined to be the band maximum.
ccc        ifeattype = -1 is an emission feature
ccc                  =  1 is an absorption band
ccc        iflag  = flag to indicate if processing image-type data:
ccc                   = 0 not image data (single spectrum analysis)
ccc                   = 1 imaging spectrometer data.
ccc        ixpxl  = x-pixel coordinate of image data (iflag = 1)
ccc        iypxl  = y-pixel coordinate of image data (iflag = 1)
ccc
ccc     OUTPUT:
ccc        rfobsc = continuum removed observed spectrum  R*4 (cr2 elements)
ccc                   NOTE: continuum is removed ONLY between cl2 and cr1
ccc                         To get complete continuum, you must remove
ccc                         it yourself using the slope and yintcp
ccc                         variables below.
ccc        xk     = k factor needed to make reference match library R*4
ccc        bd     = band depth  R*4
ccc        rfit   = fit parameter normalized to 1 channel R*4
ccc        slope  = slope to continuum of observed spectrum
ccc        yintcp = intercept to continuum of observed spectrum
ccc        rftemp = temporary working array R*4 (cr2 elements)
ccc                 at a normal return, rftemp is the fitted,
ccc                 continuum removed spectrum.
ccc        conref = continuum reflectance value of observed spectrum

	ictrol = 1
	iflag = 1
	ixpxl = 1
	iypxl = 1

        call bandmp (wav,rflibc,rfobs,cl1,cl2,cr1,cr2,ictrol,minch,
     c                  maxch,ifeattype,iflag, ixpxl,iypxl,
     c                  rfobsc,xk,bd,rfit,slope,yintcp,rftemp,conref)

1000	continue

	nn = i - 1
	write (ttyout,*) 'Bandmap called ',nn,' times'

	write (ttyout,*) 'depth=',bd,' fit=',rfit

	end
