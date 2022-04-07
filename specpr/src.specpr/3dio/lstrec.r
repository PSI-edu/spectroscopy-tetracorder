#*******************************************************************
#   TITLE:             LAST RECORD EXTRACTED                       *
#                                                                  *
#   PROGRAMMER:   Barry J. Middlebrook                             *
#                 USGS Branch of Geophysics                        *
#                 Denver West Offices                              *
#                 (303) 236-1411                                   *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This subroutine finds the number of the last       *
#               record that will be extracted from a 3d data file  *
#               during a typical linear array extraction.  This    *
#               number is used to keep the user from attempting to *
#               extract an out of bounds record and to correctly   *
#               set the protection in specpr.                      *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     .
#             a,b,c    - do loop parameters for record reading     .
#             x,y,zel  - coordinates of the line array to be con-  .
#                      sidered as the spectra                      .
#             i*,r4buff- buffer arrays for holding records         .
#                                                                  .
#...................................................................

	subroutine lstrec (in,lrec)   # SCCS ID: @(#) @(#)lstrec.r	1.3 04/18/90

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      xel,yel,zel,lrec,filhdr,reclen
	integer*4      dx,dy,dz,hdrlen,flag,in,type,rechdr,pixrec
	integer*4      orgniz,reclin
	include        "../common/label1"
	include        "../common/ioftyp"
	include        "../common/lundefs"

#  Initialize file parameter variables
	yel = filreq(1) + fiobox(1)
	xel = filreq(2) + fiobox(2)
	zel = filreq(3) + fiobox(3)
	if (xel == -2) flag = 1
	if (yel == -2) flag = 2
	if (zel == -2) flag = 3
	orgniz = filtyp(10,ftptr(in))
	filhdr = filtyp(2,ftptr(in))
	reclen = filtyp(3,ftptr(in))
	rechdr = filtyp(4,ftptr(in))
	dnoff = filtyp(5,ftptr(in))
	scale = dnscal(ftptr(in))
	dx = filtyp(7,ftptr(in))
	dy = filtyp(6,ftptr(in))
	dz = filtyp(8,ftptr(in))
	type = filtyp(9,ftptr(in))
	
#  Initialize pixels per record and records per line variables
	if (type == 1)  {
	   pixrec = reclen / 2
	}
	else if (type == 2 | type == 3)  {
	   pixrec = reclen / 4
	}

#  Select by data file organization
	if (orgniz == 1) {
		#  Band Interleaved by Line
		#  Determine pixels/record, records/line and test validity
		reclin = dx / pixrec
		k = mod(dx, pixrec)

		if (reclen == 0 | k != 0) {
	   		write (ttyout,*)'lstrec: ERROR - '
	   		write (ttyout,*)'x dimension not a multiple of ',pixrec
	   		return
		}

		#  Select extraction direction
		if (flag == 1) {
			# Piercing y-z plane
			#  Equations to control which records are read
	   		k = mod(yel, pixrec)
	   		lrec = (zel - 1) * reclin + yel / pixrec + filhdr
	   		if (k != 0) { lrec = lrec + 1 }
		} else if (flag == 2) {
			# Piercing x-z plane
			#  Equations to control which records are read 
			k = mod(xel, pixrec)
    	   		lrec = dz * reclin * (dy - 1) + (zel - 1)
			lrec = lrec + xel / pixrec + filhdr
			if (k != 0) { lrec = lrec + 1 }
		} else if (flag == 3) {
			# Piercing x-y plane
			#  Equations to read correct records
	   		k = mod(yel, pixrec)
	   		lrec = xel / pixrec + (yel - 1) * dz * reclin + filhdr
	   		if (k != 0) { lrec = lrec + 1 } 
			lrec = lrec + (dz - 1) * reclin
		} else {
			write (6,*)'lstrec: ERROR - extraction direction'
			return
		}
	} else if (orgniz == 2) {
		#  Band Interleaved by Pixel
		#  Determine pixels/record, records/line and test validity
		reclin = dz / pixrec
		k = mod(dz, pixrec)
	
		if (reclin == 0 | k != 0)  {
	   		write (ttyout,*)'lstrec: ERROR - '
	   		write (ttyout,*)'z dimension not a multiple of ',pixrec
	   		return
		}

		#  Select extraction direction
		if (flag == 1) {
			# Piercing y-z plane
			#  Equations to control which records are read
			k = mod(zel, pixrec)
	   		lrec = (yel - 1) * reclin + zel / pixrec + filhdr
	   		if (k != 0) { lrec = lrec + 1 }
			lrec = lrec + (dz - 1)
		} else if (flag == 2) {
			# Piercing x-z plane
			#  Equations to control which records are read
			k = mod(zel, pixrec)
    	   		lrec = (((dy - 1) * dx) + (xel - 1)) * reclin 
			lrec = lrec + zel / pixrec + filhdr
	   		if (k != 0) { lrec = lrec + 1 }
		} else if (flag == 3) {
			# Piercing x-y plane
			#  These equations control which records are read
	   		lrec = (yel - 1) * reclin * dx + xel * reclin
			lrec = lrec + filhdr
		} else {
			write (6,*)'lstrec: ERROR - extraction direction'
			return
		}
	} else if (orgniz == 3) {
		#  Band Sequential
		#  Determine pixels/record, records/line and test validity
		reclin = dx / pixrec
		k = mod(dx, pixrec)

		if (reclin == 0 | k != 0)  {
	    		write (ttyout,*)'lstrec: ERROR - '
	    		write (ttyout,*)'y dimension not a multiple of ',pixrec
	   		return
		}
	
		#  Select extraction direction
		if (flag == 1) {
			# Piercing y-z plane
			#  Equations to control which records are read
	   		k = mod(xel, pixrec)
    	   		lrec = (zel - 1) * reclin * dy
			lrec = lrec + xel / pixrec + filhdr
	   		if (k != 0) { lrec = lrec + 1 }
		} else if (flag == 2) {
			# Piercing x-z plane
			#  Equations to control record read loop
	   		lrec = zel * reclin * dy + filhdr
		} else if (flag == 3) {
			# Piercing x-y plane
			#  Equations to control which records are read
	                k = mod(xel, pixrec)
	   		lrec = reclin * dy * (dz - 1) + yel - 1
			lrec = lrec + xel / pixrec + filhdr
	   		if (k != 0) { lrec = lrec + 1 }
		} else {
			write (6,*)'lstrec: ERROR - extraction direction'
			return
		}
	} else {
			write (6,*)'lstrec: ERROR - Unknown file organization'
	}

# Return to calling program
	return
	end
