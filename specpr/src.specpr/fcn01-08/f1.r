	subroutine f1 (ic)
	implicit integer*4 (i-n)
#################################################################
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This routine lists the description of all the
#ccc                   special functions.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,crtin
#ccc  argument list description:
#ccc     arguments: ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
###############################################################
    include "../common/lundefs"
    include "../common/alphabet"

	ic=ihx
 	call eralph

8880	write (ttyout, 9990) 
9990	format (
        ' Special Function 1: a list of the special functions', /,
	' ---------------------------------------------------', /)

	write(ttyout, 1)
	write(ttyout, 2)
	write(ttyout, 3)
	write(ttyout, 4)
	write(ttyout, 5)
	write(ttyout, 6)
	write(ttyout, 7)
	write(ttyout, 8)
	write(ttyout, 9)
	write(ttyout,10)
	write(ttyout,11)
	write(ttyout,12)
	write(ttyout,13)
	write(ttyout,14)
	write(ttyout,15)
#
	write(ttyout,9991)
9991	format (' Press return to CONTINUE, e or x to EXIT')
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return

#
	write(ttyout,16)
	write(ttyout,17)
	write(ttyout,18)
	write(ttyout,19)
	write(ttyout,20)
	write(ttyout,21)
	write(ttyout,22)
	write(ttyout,23)
	write(ttyout,24)
	write(ttyout,25)
	write(ttyout,26)
	write(ttyout,27)
	write(ttyout,28)
	write(ttyout,29)
#
	write(ttyout,9992)
9992	format (' Press return to CONTINUE,  e or x to EXIT,',
                ' or  r  to RETURN to first page')
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return
	if (il == ihr) go to 8880

#
	write(ttyout,30)
	write(ttyout,31)
	write(ttyout,32)
	write(ttyout,33)
	write(ttyout,34)
	write(ttyout,35)
	write(ttyout,36)
	write(ttyout,37)
	write(ttyout,38)
	write(ttyout,39)

#
	write(ttyout,9992)
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return
	if (il == ihr) go to 8880

#

	write (ttyout, 9993)
9993	format (' Functions 40-59 developed by USGS, Denver',/,
                ' -----------------------------------------')

	write(ttyout,40)
	write(ttyout,41)
	write(ttyout,42)
	write(ttyout,43)
	write(ttyout,44)
	write(ttyout,45)
	write(ttyout,46)
	write(ttyout,47)
	write(ttyout,48)
	write(ttyout,49)
#
	write(ttyout,9992)
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return
	if (il == ihr) go to 8880

#
	write (ttyout, 9993)

	write(ttyout,50)
	write(ttyout,51)
	write(ttyout,52)
	write(ttyout,53)
	write(ttyout,54)
	write(ttyout,55)
	write(ttyout,56)
	write(ttyout,57)
	write(ttyout,58)
	write(ttyout,59)
#
	write(ttyout,9992)
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return
	if (il == ihr) go to 8880

	write (ttyout, 9994)
9994	format (' Functions 60-69 developed by USGS, Reston',/,
                ' -----------------------------------------')

	write(ttyout,60)
	write(ttyout,61)
	write(ttyout,62)
	write(ttyout,63)
	write(ttyout,64)
	write(ttyout,65)
	write(ttyout,66)
	write(ttyout,67)
	write(ttyout,68)
	write(ttyout,69)
#

	write(ttyout,9992)
	call crtin
	i=1
	call wjfren (i,x,il)
	if ((il == ihx) .or. (il == ihe)) return
	if (il == ihr) go to 8880

#	write (ttyout,9995)
9995	format (' Functions 70-79 developed by U. Wash., Seattle',/,
                ' ----------------------------------------------')


#
	return
1	format('f 1 - list        : Description of all special function ',
		   'routines')
2	format('f 2 - shif[t]     : SHIFTS data left or right a number',
			'of channels (f,e)')
3	format('f 3 - sequ        : SETS UP SEQUENTIAL PROCESSING for the',
		   ' user')
4	format('f 4 - exse[qu]    : executes SEQUENTIAL PROCESSING set ',
		   'up by f3. No user access.')
5	format('f 5 - cont[rem]   : CONTINUUM REMOVAL (use also f12)')
6	format('f 6 - plan[ck]    : computes PLANCK BLACK BODY function')
7	format('f 7 - smoo[th]    : SMOOTHING function  (f,e)')
8	format('f 8 - tran[spose] : channel-file TRANSPOSE')
9	format('f 9 - band[rem]   : BAND REMOVAL (reflection method)')
10	format('f10 - sort        : SORTs channels into increasing ',
		   'wavelength order (f,e)')
11	format('f11 - luna[rtherm]: Lunar THERMAL REMOVAL (f,e)')
12	format('f12 - cspl[in]    : CUBIC SPLINE interpolation to ',
		   'new wavelength set,  (f,e)')
13	format('f13 - merg[e]     : MERGes 2 files (f,e)')
14	format('f14 - edit        : EDITs file and errors (f,e)')
15	format('f15 - gaus[sfmt]  : FORMATS Gaussian parameter file (f)' )
16	format('f16 - line[gen]   : user LINE SEGMENT GENERATOR')
17	format('f17 - conv[olv]   : high to low resolution ',
					'SPECTRAL CONVOLUTION (f).')
18	format('f18 - bloc[kav]   : BLOCK AVERAGES and Statistics (f,e).')
19	format('f19 - poly[fit]   : POLYNOMIAL FIT routine (f,e)')
20	format('f20 - read        : READ/WRITE data from/to a TEXT file')
21	format('f21 - calc[poly]  : Calculate N term POLYNOMIAL (f)')
22	format('f22 - digi[tize]  : DIGITIZATION tablet input routine')
23	format('f23 - pars[e]     : MATH PARSER')
24	format('f24 - star[moontherm] : star/moon-spot reflectance ',
		   'THERMAL REMOVAL (f,e)')
25	format('f25 - twoc[omlsq] : Two-Component LEAST SQUARE ',
		   'ANALYSIS (f); WARNING: not debugged')
26	format('f26 - not available yet')
27	format('f27 - not available yet')
28	format('f28 - not available yet')
29	format('f29 - not available yet')
30	format('f30 - not available yet')
31	format('f31 - not available yet')
32	format('f32 - not available yet')
33	format('f33 - not available yet')
34	format('f34 - not available yet')
35	format('f35 - not available yet')
36	format('f36 -            : f42 alternate algorithm (use f42)')
37	format('f37 - nvres      : edge position algorithm (f)')
38	format('f38 - km         : Kubelka-Munk Remission function (f)')
39	format('f39 - random     : normal random number generator',
			' (mean 0, variance 1')
# USGS Denver routines:

40	format('f40 - leastsq     : compute LEAST SQUARES between ',
						'two spectra (f) under devlp.')
41	format('f41 - bin         : BINNING routine (f) under development')
42	format('f42 - bandmap     : Map band depths and fit from a reference',/,
               '                        spectrum (f)')
43	format('f43 - fft         : FFT and Inverse FFT')
44	format('f44 - suh         : Segmented Upper Hull automatic ',
					'CONTINUUM ANALYSIS (f,e)')
45	format('f45 - aba         : Automatic BAND ANALYSIS (f,e)')
46	format('f46 - abaout	  : Band Analysis output to a ',
						'binary FILE (f)')
47	format('f47 - abaspecrec  : Band Analysis SPECTRUM RECREATION',
								' (f)')
48	format('f48 - hpdigit     : DIGITIZATION from an HP ',
							'Terminal/Tablet')
49	format('f49 - lininterp   : Linear interpolation (f,e)')
50	format('f50 -             : wavelength registration')
51	format('f51 - ccon        : Curved continuum removal over ',/,
					'a spectral',/,
					' feature (f) under development')
52	format('f52 - cbandmap    : Map band depths and fit from ',/,
				'a reference spectrum using automated ',/,
				'curved continuaa (f) under development')
53	format('f53 - not available yet')
54	format('f54 - not available yet')
55	format('f55 - not available yet')
56	format('f56 - not available yet')
57	format('f57 - not available yet')
58	format('f58 - not available yet')
59	format('f59 - not available yet')

# USGS Reston routines:

60	format('f60 - not available yet')
61	format('f61 - not available yet')
62	format('f62 - not available yet')
63	format('f63 - not available yet')
64	format('f64 - not available yet')
65	format('f65 - not available yet')
66	format('f66 - not available yet')
67	format('f67 - not available yet')
68	format('f68 - not available yet')
69	format('f69 - not available yet')

8999	format (a,'  not available yet')
		end
