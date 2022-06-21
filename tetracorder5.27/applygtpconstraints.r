	subroutine applygtpconstraints

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         applygtpconstraints
#ccc  version date: 09/19/2020
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: disables materials globally if
#ccc                     incominfg data temperature and pressure
#ccc                     ranges are outside the range for a
#ccc                     material.
#ccc                     This is global, and not per pixel.
#ccc
#ccc
#ccc
#ccc  algorithm description: 
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description: see below
#ccc  parameter description: see below
#ccc  common description: see below
#ccc  message files referenced: none
#ccc  internal variables: see below
#ccc  file description: see below
#ccc  user command lines: see below
#ccc  update information: see below
#ccc  NOTES:
#ccc
#ccc         imat = material number (see multmap.h
#ccc         xdat1sp = spectrum to analyze

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"

# arrays for multiple materials

	include "multmap.h"


	integer*4 tmplength, imat

        integer*4 lnb     # function

	character*40 tmptitle

	write(ttyout,1)
1	format (' Checking pressure and temperature constraints for each material')

	write(ttyout,2) dtemperature(1), dtemperature(2)
	write(ttyout,3) dpressure(1),    dpressure(2)
2	format ('           input data temperature range: ',f6.1,' to ',f6.1,' Kelvin')
3	format ('           input data  pressure   range: ',f6.1,' to ',f6.1,' Bars')

	# NOTE: if mpressure, mtemp havd not been specirfied, the value is -999.

	do imat = 1, nmats {

	    if ( imatenable(imat) == 1 ) {   # do temperature, pressure checks

		write (ttyout,*) 'DEBUG applygtpconstraints, material:', imat,'  mpressure= ', mpressure(1,imat),' to ',mpressure(4,imat)
		write (ttyout,*) 'DEBUG applygtpconstraints, material:', imat,' temperature=', mtemp(1,imat),' to ',mtemp(4,imat)
		tmptitle = mfile(imat)(1:30)
		tmplength = lnb(mfile(imat)(1:30))

		# if the max data pressure is less than material min P, disable
		if ( dpressure(2) < mpressure(1,imat)  && mpressure(1,imat) > -0.00001 ) {

			imatenable(imat) = 0
			write (ttyout, 101) imat, tmptitle(1:tmplength),
				mpressure(1,imat), dpressure(1)
		}
		
		# if the lowest data Pressure is greater than material max P, disable
		if ( dpressure(1) > mpressure(4,imat)  && mpressure(4,imat) > -0.00001 ) {

			imatenable(imat) = 0
			write (ttyout, 102) imat, tmptitle(1:tmplength),
				mpressure(4,imat), dpressure(2)
		}
		
		# if the max data temperature is less than material min T, disable
		if ( dtemperature(2) < mtemp(1,imat)  &&  mtemp(1,imat) > -0.00001 ) {

			imatenable(imat) = 0
			write (ttyout, 201) imat, tmptitle(1:tmplength),
				mtemp(1,imat), dtemperature(1)
		}
		
		# if the lowest data temperature is greater than material max T, disable
		if ( dtemperature(1) > mtemp(4,imat)  &&  mtemp(4,imat) > -0.00001 ) {

			imatenable(imat) = 0
			write (ttyout, 202) imat, tmptitle(1:tmplength),
				mtemp(4,imat), dtemperature(2)
		}
	    }
	}
101	format('   Material',i6,' DISABLED pressure out of range low  ',a,
			'  mat-pressure-min=', f9.4,' datapressuremin=',f9.4)

102	format('   Material',i6,' DISABLED pressure out of range high  ',a,
			'  mat-pressure-max=', f9.4,' datapressuremax=',f9.4)

201	format('   Material',i6,' DISABLED temperature out of range low  ',a,
			'  mat-temperature-min=', f9.4,' datatemperaturemin=',f9.4)

202	format('   Material',i6,' DISABLED temperature out of range high  ',a,
			'  mat-temperature-max=', f9.4,' datatemperaturemax=',f9.4)

4050 	return
	end

