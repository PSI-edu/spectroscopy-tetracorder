   subroutine getn2(jlyr, iminr,il,mmixflag)

#  	gets index of refraction from specpr file

	implicit integer*4 (i-n)
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "defs.h"
	include "lmrefl.h"

	integer*4 mmixflag, jloop, mlayers, jlyr

	mlayers = 1           # temp variable for layers in actual use
	if ( nlayers > 1 ) {
		mlayers = nlayers
	}


10	if (nlayers > 1) {
		write (ttyout, 19) jlyr
19		format (/, '############### LAYER ', i2,' ########################')
		write (ttyout, 20) jlyr, iminr
20		format (/,'************** Layer',i3,' Mixture Component',i3,' ************',/)
	} else {
		write (ttyout, 21) iminr
21		format (/,'************** Mixture Component',i3,' ************',/)

	}

	write (ttyout, 2222)
2222	format (' Example structure of the input needed in this section:',/,
                /,
                '     s = single material component',/,
                '            1st line: INDXREF of the main component 1  (e.g. hematite)',/,
                '            2nd line: ABSCOEF of the main component 1 (include f to float the abundance)',/,
                '            3rd line: grain size weight fraction  and density',/,
                /,
                '     m 2 = number of components for a multi-component molecular mix (example 2)',/,
                '            1st line: INDXREF of the main component 1 (e.g. hematite)',/,
                '            2nd line: INDXREF embedded component 2    (e.g. CO2 that is trapped in the hematite)',/,
                '            3rd line: ABSCOEF of the main component 1 (include f to float the abundance)',/,
                '            4th line: ABSCOEF embedded component 2 and abundance',/,
                '            5th line: grain size weight fraction  and density',/,
                /,
                '     f 2 =  a 2-component effective medium molecular mix (example 2)',/,
                '            1st line: INDXREF of the main component 1 (e.g. hematite)',/,
                '            2nd line: INDXREF embedded component 2    (e.g. CO2 that is trapped in the hematite)',/,
                '            3rd line: ABSCOEF of the main component 1 (include f to float the abundance)',/,
                '            4th line: ABSCOEF embedded component 2 and abundance',/,
                '            5th line: grain size weight fraction  and density',/,
                /,
                '     b 3 =  effective medium + multi-component molecular mix for 3+ component mix',/,
                '            1st line: INDXREF of the main component 1  (e.g. hematite)',/,
                '            2nd line: INDXREF embedded component 2   (e.g. CO2 that is trapped in the hematite)',/,
                '            3rd line: INDXREF embedded component 3   (e.g. OD)',/,
                '            4th line: ABSCOEF of the main component 1 (include f to float the abundance)',/,
                '            5th line: ABSCOEF embedded component 2 and abundance',/,
                '            6th line: ABSCOEF embedded component 3 and abundance',/,
                '            7th line: grain size weight fraction  and density',/,
		/)


	write (ttyout, 22) NMMIX, NMMIX
22	format (' Type  s  to enter a single set of n and k or',/,
                  '       m  and the number of components for a ',
				'multi-component molecular mix',/,
		  '           (max =',i3,') (linear mix abs coefs)',/,
		  '       f 2  for a 2-component',
                                ' effective medium molecular mix',/,
		  '          e.g.: a space weathered silicate',/,
		  '          Make the 2nd component the small fraction',/,
		  '       b #  effective medium + multi-component',/,
		  '          molecular mix for #= 3 to ',i3,' component mix',/,
		  ' WAVELENGTHS MUST BE IN MICRONS',//,
		  ' Follow the above with  m (or matrix)  to declare ',
				'this index set the MATRIX MATERIAL')

	mmixflag=0  	# default: =0 = no molecular mixture
			# mmixflag =1 = molecular mix of abs coefs
			# mmixflag =2 = effective medium mix
			# mmixflag =3 = effective medium mix + molecular mix of abs coefs
			#               effective medium mix is only component 2 of the mix

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) {
		il = id
		go to 10000
	}

	if (il == ihs) {   # enter single set of optical constants

            call wjfren (i,x,il)
            if (il == ihm) {         # this material is the matrix
		matrixptr = iminr
            }

100	  write (ttyout, 105) iminr
105	  format (/,'============',/,
                ' type in the file ids and record numbers of the ',
		' real INDEX of REFRACTION,',/,
		' for element ',i2)

	  call crtin
	  i = 1
	  call wjfren (i,x,id)
	  call wjfren (i,x,il)
	  if (id == ihe || id == ihx) {
		il = id
		go to 10000
	  }
	  if (il == ihe || il == ihx) go to 10000
	  if (il != 0) {
		call what(i)
		go to 100
	  }
	  if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
50	        format (' ERROR record number out of range, reenter',/)
		go to 100
	  }
	  irecxn(iminr)          = x    #record number for xn set iminr
	  irecxnlyr(jlyr, iminr) = x
	  idxn(iminr) = id
	  irecxklyr(jlyr, iminr) = id
	  call devok(4,id,irecxn(iminr),lun, ier)
	  if (ier != 0) go to 100
	  itmp = irecxn(iminr)
	  call redfil (itmp,lun,ier)
	  if (ier != 0) go to 100
	  write (ttyout,110) iminr, ititl, itchan
110	  format (/, ' Index of Refraction set',i3,/, 5x,
		a, 5x, 'channels=',i6,/)
	  do j = 1, nchans {
		xn(iminr,j)         = data(j)
		xnlyr(jlyr,iminr,j) = data(j)
	  }
	  xntitl(iminr)         = ititl
	  xntitllyr(jlyr,iminr) = ititl

	} else if (il == ihm || il == ihf || il == ihb ) {  # molecular mix or effective medium mix


	    if (il == ihm) mmixflag=1  # linear absorption coefficient mix
	    if (il == ihf) mmixflag=2  # effective medium mix
	    if (il == ihb) mmixflag=3  # effective medium mix + linear absorption coefficient mix

	    itmp=0
	    if (mmixflag == 1 || mmixflag == 3) {      # linear absorption coefficient mix or both linear + effective
	    	call wjfren (i,x,il)  # get the nimner in the molecular mix
	    	if (il == ihe || il == ihx) go to 10000
            	if (il != 0) {
                  call what(i)
                  go to 10
            	}
	    	itmp = x
	    }
	    if (mmixflag == 2) { # effective medium of 2 components
		itmp=2           # limit to only 2 at this time
	    }
	    if (itmp < 1 || itmp > NMMIX ) {

		write (ttyout, 40) itmp
40		format (' ERROR: number of grain internal mixtures',i4,
				' out of range, reenter',/)
		go to 10
	    }
	    nmolmix(iminr)         = itmp   # number of molecular mixtures in component iminr
	    nmolmixlyr(jlyr,iminr) = itmp   # number of molecular mixtures in component iminr


            call wjfren (i,x,il)     # check for point that says this is the matrix material
            if (il == ihm) {         # this material is the matrix
		matrixptr = iminr
            }

	    do jloop = 1, nmolmix(iminr) {

500		write (ttyout, 505) iminr, jloop
505		format (/,
			' type in the file ids and record numbers of the ',
			' real INDEX of REFRACTION,',/,
			' for element ',i2,/,
			' for component ',i3,' to be mixed in the grain')

		if ( jloop == 1 ) {

		  write (ttyout, 506)
506		  format (' This is the main component of the embedded mixture')
		}
		if ( mmixflag == 3 && jloop == 2 || mmixflag == 2) {

		  write (ttyout, 507)
507		  format (' This is the embedded particles for the effective medium',/,
			  ' (e.g. trace nano-phase iron index of refraction)')
		} else if ( mmixflag == 3 && jloop > 2 || mmixflag == 1 && mmixflag == 2 ) {

		  write (ttyout, 508)
508		  format (' This is the molecular mix, e.g. isotope or trace compound')
		}

		call crtin
		i = 1
		call wjfren (i,x,id)
		call wjfren (i,x,il)
		if (id == ihe || id == ihx) {
			il = id
			go to 10000
		}
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 500
		}
		if (x < 1 || x > maxrec) {
			write (ttyout, 50) 
			go to 500
		}
		#if (jloop == 1 ) {           # keep info on first entry
			irecxn(iminr)         = x     #record number for xn set iminr
			irecxnlyr(jlyr,iminr) = x     #record number for xn set iminr
			idxn(iminr)           = id
			idxnlyr(jlyr,iminr)   = id
		#}
		call devok(4,id,irecxn(iminr),lun, ier)
		if (ier != 0) go to 500
		itmp = irecxn(iminr)
		call redfil (itmp,lun,ier)
		if (ier != 0) go to 500
		write (ttyout,111) iminr, jloop, ititl, itchan
111	  	format (/, ' Index of Refraction set',i3,
			' mix',i3,/, 5x,a, 5x, 'channels=',i6,/)
		do j = 1, nchans {
			xmindx(jloop,iminr,j)         = data(j)
			xmindxlyr(ljyr,jloop,iminr,j) = data(j)
		}
		if (jloop == 1 ) {    # keep info on first entry
			xntitl(iminr)         = ititl
			xntitllyr(jlyr,iminr) = ititl
		}
	    }

	} else {

		write (ttyout,200)
200		format (' Input not recognized (subroutine getn2).  Re-enter',//)
		go to 10
	}


10000	return
	end 
