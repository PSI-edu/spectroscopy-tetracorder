	subroutine cubecorder

	implicit integer*4 (i-n)
######	implicit none

#ccc  name:         reflsetup
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: maps a cube with tetracorder algorithm
#ccc                     for tetracorder to map with.
#ccc
#ccc  algorithm description: See Clark et al, 1990, JPL AVIRIS Conf.
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
#ccc            This routine generates a band depth map

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl3"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lbl7"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/ioftyp"
	include 	"../specpr/src.specpr/common/blank"
	include	"../specpr/src.specpr/common/lblvol"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

# arrays for buffering output

	include "obuffers.h"

	integer*4 ihour, imm, deg, minute, mm, dd, yy
	integer*4 numtyp, imat, ibitspxl, lblsiz, iorg
	integer*4 ioutbnds, length, lengthd, il, inth, idiaflg
	integer*4 ifeat, ibest, jj, ig0flg, ikl
	real*4    x
	real*4    ss
	real*8    xjda
	real*4    xtmp
	integer*4 lnb, fnb

	character*1 imch(5)
	character*80 icube   # cube file name
	character*80 chtmp
	character*200 chtmp2

	integer*4 cmdverbose   # function cmdverbose

# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

# Now get the 3d file id

4	if (cmdverbose(-1) <= 1) write (ttyout,401)
401	format (///,' Type in the specpr letter ID of the 3D ',
		'IMAGING SPECTROMETER DATA SET',/,
		'      (lower case letter v, w, d, u, or y).',/,
		'or type  cube: and the cube file name',/,
		'               (the cube must be vicar format)')

# Get 3d file id
	call crtin
	i=1
	call wjfren (i,x,f3did)

	if (f3did == ihe | f3did == ihx) {
		icrst = 1
		call rstart (icrst)
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	}

#  Check device (3d file id) validity and get device unit number
#     or get from file directly

	if (f3did == ihc & i < 75 & i >1) { # specify own cube
		i = i-1
		if (iopcon(i:i+4) == 'cube:') {
			i= i+5
			chtmp = iopcon(i:80)
			itmp1 = fnb(chtmp)
			itmp2 = lnb(chtmp)
			#write (ttyout,*) 'DEBUG:',iopcon
			#write (ttyout,*) 'DEBUG:',chtmp
			#write (ttyout,*) 'DEBUG:',itmp1, itmp2
			if (itmp1 == 0 | itmp2 == 0 | itmp2 < itmp1) {
				call what(itmp1+i)
				call what(itmp2+i)
				go to 4
			}
			#write (ttyout,*) 'DEBUG:file="',chtmp(itmp1:itmp2),'"'
			icube=chtmp(itmp1:itmp2)
			# cube logical unit, qlun, defined in tetracorder.r
			call opencube (qlun, icube, filhdr,
					reclen, rechdr,
					dx, dy, dz,
					numtyp, filorg, ier)
			if (ier != 0) {
				go to 4
			}

			write (ttyout,*) 'Cube opened.  file='
			write (ttyout,*) icube(1:lnb(icube))

			write (ttyout,*) '    IMAGE CUBE:  filhdr=',filhdr,
                                        'reclen=',reclen,
					'rechdr=',rechdr
			write (ttyout,*) '    IMAGE CUBE:  lines=',dy,
                                        'samples=',dx,
					'bands=',dz

			if ( dz > 30 ) {      # which channel to monitor during the run
				monchan = 20
			} else {

				monchan = dz / 2
			}

			# filorg =1  = BIL
			# filorg =2  = BIP
			# filorg =3  = BSQ

			if ( filorg == 1 ) {
				write (ttyout,*) '    IMAGE CUBE: organization= BIL, filorg=',filorg
			} else if ( filorg == 2 ) {
				write (ttyout,*) '    IMAGE CUBE: organization= BIP, filorg=',filorg
			} else if ( filorg == 3 ) {
				write (ttyout,*) '    IMAGE CUBE: organization= BSQ, filorg=',filorg
			} else  {
				write (ttyout,*) '    IMAGE CUBE: organization= UNKNOWN, filorg=',filorg
			}

			# numtyp =1  = 16-bit signed integers
			# numtyp =2  = 32-bit signed integers
			# numtyp =3  = 32-bit floating point

			if ( numtyp == 1) {
				write (ttyout,*) '    IMAGE CUBE: type= 16-bit signed integers, nymtyp=', numtyp
			} else if ( numtyp == 2) {
				write (ttyout,*) '    IMAGE CUBE: type= 32-bit signed integers, nymtyp=', numtyp
			} else if ( numtyp == 3) {
				write (ttyout,*) '    IMAGE CUBE: type= 32-bit floating point, nymtyp=', numtyp
			} else {
				write (ttyout,*) '    IMAGE CUBE: type= UNKNOWN, nymtyp=', numtyp
			}
			


			write(chtmp2,*) 'tetracorder', iversion, ' ',
					icube(1:lnb(icube)),
					' lines=',dy, ' samples=',dx,
					' bands=',dz


			call system ('/usr/local/bin/logtetracorder' // chtmp2(1:lnb(chtmp2)) // char(0))

			idnoff = 0
			iptdrop = -32767
			scale=20000
			write (ttyout, 243)
243			format (/,'Enter the DN offset, ',
				'Deleted point value, and the ',
				'scale factor (to scale the DN to reflectance',/,
				5x,'and the minumum and maximum data thresholds.',//,
				5x,'The thresholds are in real number (scaled) values, and',/,
				5x,'if = 0.0  no thresholds are set')
			call crtin
			i = 1
			call wjfren (i,x,il)

			if (il == ihe | il == ihx) {
				icrst = 1
				call rstart (icrst)
				write (ttyout,*)'Exiting.'
				call what (-1)
				return
			}
			if (il != 0) {
				call what (i)
				go to 4
			}
			idnoff = x + 0.5

			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				go to 4
			}
			iptdrop = x + 0.5

			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				go to 4
			}
			scale = x
			
			# now get thresholds, if present (added Jan 3, 1997, Roger Clark)

			thrshmin=-1.0e35
			thrshmax= 1.0e35

			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				go to 4
			}
			if(abs(x) > 0.1e-20) thrshmin = x

			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				go to 4
			}
			if(abs(x) > 0.1e-20) thrshmax = x
			

#                       write history

			write (lunhist,3020) icube(1:lnb(icube))
3020			format ('cube:',a)

			write (lunhist,3021) idnoff, iptdrop,
						scale, ihbcksl
3021			format (i6,1x,i9,1x,f13.6,5x,a1,
					'# offset, dltd pt, scale')
			
			write (lunresult,3020) icube(1:lnb(icube))
			write (lunresult,3022) idnoff, iptdrop,
						scale, ihbcksl, thrshmin, thrshmax
3022                    format (i6,1x,i9,1x,f13.6,5x,a1, f13.6, f13.6,
                                        '# offset, dltd pt, scale, thrshmin, thrshmax')

		}
			
	} else {
#                        **  Cube is assigned in specpr **

		call devok (4,f3did,1,qlun,ier) 

		if (ier != 0) {
			write (ttyout,*)
			write (ttyout,403)
403			format ('ERROR: enter a valid ID',//)
			call what (-1)
			go to 4
		}

# 		write history
		write (lunhist,3030) f3did,ihbcksl
3030		format (a1,21x,a,'# specpr 3D file ID')

#		Initalize 3d file parameter variables: given logical unit,
#		qlun, find all the headers, dimensions, etc of the 3d file.

		call setupreadsheet (qlun, filhdr, reclen, rechdr,
			idnoff, dx, dy, dz, numtyp, filorg,
			iptdrop, scale)
	}

#	if (numtyp != 1 || filorg != 1) {
#		write (ttyout,413)
#413		format ('ERROR: the only valid file type is',
#			' BIL, Integer*2.',/,
#			' Current file type =',i3,
#			'   Organization =',i3,//)
#		go to 4
#	}
#	if (rechdr > 0) {
#		write (ttyout,*) 'Program is not yet set up to work',
#				'with record headers > 0.  rechdr=',rechdr
#		go to 4
#	}
	if (dx < 1 || dy < 1 || dz < 1) {
		write (ttyout,414) dy,dx,dz
414		format (' ERROR: dimensions invalid:',
			'lines:',i5,'   samples:',i5,'   bands:',i5,//)
		go to 4
	}
	if (dx > maxpix ) {
		write (ttyout,415) dx, maxpix
415		format (' ERROR: number of samples (',i6,') is larger',
			' than maximum (',i6,') allowed',//)
		go to 4
	}

#       compute number of header records

        nrecshdr = filhdr

	if (cmdverbose(-1) <= 2) write (ttyout,416) dy,dx,dz
416	format (' Imaging spectrometer data:',/,
		28x,'Lines:',i5,'   Samples:',i5,'   Bands:',i5,/)
        write (ttyout,*) 'Note: found ',nrecshdr,' header records'

	write (lunhist, 3040) ihbcksl, dy, dx, dz
3040	format (a,'#',23x,'3D data set: Lines:',i5,
		'   Samples:',i5,'   Bands:',i5)
	write (lunhist,3111) ihbcksl
3111	format (a,'#',70('#'))

################
#
# now open tetracorder-format output cube and create header
#
################

	ier = 0
	# call opentf (ier)     # thi is for the future
	if (ier != 0) {
		write (ttyout, *) 'PRESS RETURN TO EXIT'
		call what (-1)
		call crtin
		return
	}

########################

	do i = 1, maxmat {
		izeroline(i) = 0
		nzeroline(i) = 0
		olinb(i) = 0       # output buffer line number
	}


	flag = 1
	bdrecl = dx
	reclin = float((2*dx)/reclen)


###########################################################
#### primary setup do loop for file names #################

	if (nzgroup == 0) {

		write (ttyout,*) "WARNING: no groups!  nzgroup = ", nzgroup
	}

	do igroup = 1, nzgroup {  # Create each output file by group

		if (nmatgrp(igroup) == 0) {

			write (ttyout,*) "NO materials on group ",igroup," not creating outout files for this group"
			next
		}

		ig0flg = 0    # first do not do group 0 output
		do imat = 1, nmatgrp(igroup) {

			#write (ttyout,*) 'DEBUG: creating files for group',igroup

			kcase = 0
			call creatoutfiles(igroup,kcase,matgrp(imat,igroup),ig0flg)

			if (ier != 0) {    # note ier is passed in common

				write (ttyout,*) 'Press return to exit'
				call crtin
				return
			}
		}

		if (incgrp0(igroup) == 1 & nmatgrp(0) > 0) { # include group 0

			ig0flg = 1    # now do group 0 output for group igroup
			if (nmatgrp(0) > 0) {
				do imat = 1, nmatgrp(0) {
					kcase = 0
					call creatoutfiles(igroup,kcase,
						matgrp(imat,0),ig0flg)
				}
			}
		}

	} # end of create outout file loop for groups

	do kcase = 1, nzcase {   # now do cases

		if (nmatcse(kcase) == 0) next

		do imat = 1, nmatcse(kcase) {

			#write (ttyout,*) 'DEBUG: creating files for case', kcase

			igroup = -1
			ig0flg = 0
			call creatoutfiles(igroup,kcase,matcse(imat,kcase),ig0flg)

			if (ier != 0) {    # note ier is passed in common

				write (ttyout,*) 'Press return to exit'
				call crtin
				return
			}

		}
	}


# Set error message toggle on or off
8	if (cmdverbose(-1) <= 1) {
		write (ttyout,223)
223		format (/,'If an error occurs the output is ',
				'set to a deleted',/,
			'point value.  Please set the error ',
				'message option:',//,
			'0	DO NOT PRINT error messages.',//,
			'1	PRINT error messages.',
			/)
	}
	
# Get input 
	call crtin
	i=1
	call wjfren (i,x,il)

	if (il == ihe | il == ihx)
	{
		icrst = 1
		call rstart (icrst)
		call closef
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	}

	emtogl = nint(x)

	if (emtogl != 0 & emtogl != 1) {
		call what (i)
		write (ttyout,*)'Invalid response.  Please re-enter.'
		write (ttyout,*)
		go to 8
	}

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*)'Error msg toggle = ',emtogl
	}

# write history
	write (lunhist, 3120) emtogl, ihbcksl
3120	format (i1,22x, a,'# Error message toggle flag')
	
# Get interval to print out values being calculated in do loop

9	if (cmdverbose(-1) <= 1) {
		write (ttyout,*)
3121		format (/,
		'The loop calculating the band depth and fit',/,
		'will print out the calculated values every nth line.',
		/,
		'In addition, you can flag for full diagnostic ',/,
		'printout or normal printout.',//,
		'Diagnostic flag values:',//,
		'0	Print pixel coordinates, band depth, ',
							'error fit,',/,
		'	and scaled (8 bit) band depth and error fit.',
		//,'1	Print wavelengths, continuum removed ',
							'reference,',/,
		'	observed reflectance, continuum removed ob-',/,
		'	served reflectance, beginning and end band ',/,
		'	ranges, pixel coordinates, band depth, ',
							'quality',/,
		'	of  fit and scaled (8 bit) band depth and ',/,
		'	quality of fit.',//,
		'Enter n and diagnostics flag:',/,
		'n   flag:')
	}

# Get nth variable value
	call crtin
	i=1
	call wjfren (i,x,il)

	if (il == ihe | il == ihx)
	{
		icrst = 1
		call rstart (icrst)
		call closef
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	}

	nth = nint(x)

# Get diagnostics flag
	call wjfren (i,x,il)

	if (il == ihe | il == ihx)
	{
		icrst = 1
		call rstart (icrst)
		call closef
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	}

	diaflg = nint(x)

# Check and verify diagnostics flag value
	if (diaflg != 0 & diaflg != 1)
	{
		call what(-1)
		write (ttyout,*)'ERROR: Diagnostics flag must = 1 or 0.'
		write (ttyout,*)'Please re-enter.'
		write (ttyout,*)
		go to 9
	}
	else if (diaflg == 0)
	{
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*)
			write (ttyout,*)'Limited diagnostic ',
						'output selected.'
			write (ttyout,*)
		}
	}
	else
	{
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*)
			write (ttyout,*)'Full diagnostic ',
						'output selected.'
			write (ttyout,*)
		}
	}

# Check and verify nth variable value
	if (nth <= 0 | nth > dy)
	{
		call what(-1)
		write (ttyout,*)'ERROR: Out of range value for n.'
		write (ttyout,*)'Please re-enter.'
		write (ttyout,*)
		go to 8
	}
	else
	{
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*)
			write (ttyout,*)'Information printed every ',
						nth,'th line.'
			write (ttyout,*)
		}
	}

# write history
	inth = nth
	idiaflg = diaflg
	write (lunhist,3130) nth, diaflg, ihbcksl, inth, idiaflg
3130	format (i4,2x,i2,15x,a,'# print every',i5,
		' with diagnostic flag=',i2)


	if (cmdverbose(-1) <= 1) {
		write (ttyout, *) 'Flushing history and results buffer'
	}
	call flushseqfile (lunhist, histfile(1:lnb(histfile)), ier)
	if (ier != 0) {
		call what (-1)
		return
	}
	call flushseqfile (lunresult, resultfile(1:lnb(resultfile)), ier)
	if (ier != 0) {
		call what (-1)
		return
	}

# set stats to zero

	do i = 1, nmats {
		statsmapfit(i) = 0
		statsmapdepth(i) = 0
		statsmapfd(i) = 0
		fitmean(i) = 0
	}

	icubflg = 1  # process a cube

# Loop to extract spectra from 3d data file and compute band depth
	nrec = 0
	dx2 = dx / 2

	if (cmdverbose(-1) <= 2) write (ttyout,*) 'WORKING...wait'

	do yel = 1,dy {    # process a line at a time, yel - line number

		#write (ttyout,*) 'DEBUG: starting line ',yel
		#write (ttyout,*) 'DEBUG: reading channels ',chfirst,' to ',chlast
		#write (ttyout,*) 'DEBUG: qlun=',qlun,' reclen=',reclen,' nrecshdr=',nrecshdr,
                #                         'dx, dy, dz=',dx, dy, dz,' idnoff=',idnoff,
		#			' scale=',scale,' iptdrop=',iptdrop

#
#               read a spectral sheet and put in BIP, Real*4 format
#
		call read2sheet(qlun,reclen,nrecshdr,
				dx,dy,dz,idnoff,scale,iptdrop,
				chfirst,chlast,yel,filorg,numtyp,
				specdat,ioerr)

		#write (ttyout,*) 'DEBUG: read2sheet complete'

		if (ioerr != 0) {
			call what (-1)
			return
		}
	
		do xel = 1,dx {     # Process each spectrum in sheet

			#write (ttyout,*) 'DEBUG: starting cross track spectral processing',xel

			# check thresholds and delete out of min max range

			if (thrshmin > -1.0e34 | thrshmax < 1.0e34 ) {  # do range check
				do ikl = 1, nchans {
					if (specdat(ikl,xel) < thrshmin) {
						specdat(ikl,xel) = -1.23e34
						#write (ttyout,*) 'DEBUG: pxl(S,L):',
						#		xel,yel,' ch:',ikl,
						#		' below threshold: DELETED'
					}
					if (specdat(ikl,xel) > thrshmax) {
						specdat(ikl,xel) = -1.23e34
						#write (ttyout,*) 'DEBUG: pxl(S,L):',
						#		xel,yel,' ch:',ikl,
						#		' above threshold: DELETED'
					}
				}
			}

			######## analyze the spectrum ############

#		write(*,*) 'DEBUG: ', icubflg, specdat(nchans/2,xel) # refl value
			call tp1all (icubflg,specdat(1,xel))
#		write(*,*) 'DEBUG: ', icubflg, specdat(nchans/2,xel)

			##########################################

			do jtmp = 1, nzgroup {    # store best match for each
							# group and pixel
				igbest(jtmp,xel) = grpbest(jtmp)
#		write(*,*) 'DEBUG: best match', yel, xel, igbest(jtmp,xel) # material No.
			}

		}  # end analysis of one sheet

#                    now output sheet

		#write (ttyout,*) 'DEBUG: starting output of image lines'

		do igroup = 1, nzgroup {  # output each material by group
#		write(*,*) 'DEBUG: ', igroup, nmatgrp(igroup)
			if (nmatgrp(igroup) == 0) next

			ig0flg = 0    # first do not do group 0 output
			kcase = 0
			do imat = 1, nmatgrp(igroup) {
#			write(*,*) 'DEBUG: ', igroup, imat, matgrp(imat, igroup)

				call cublineout(igroup,kcase,matgrp(imat,igroup),ig0flg)

				if (ier != 0) {    # note ier is passed in common

					write (ttyout,*) 'Press return to exit'
					call crtin
					return
				}
			}

			if (incgrp0(igroup) == 1 & nmatgrp(0) > 0) { # include group 0

				ig0flg = 1    # now do group 0 output for group igroup
				   do imat = 1, nmatgrp(0) {
					call cublineout(igroup,kcase,
						matgrp(imat,0),ig0flg)
				   }
			}

		} # end Output of results loop for groups
		do kcase = 1, nzcase {   # now do cases

			if (nmatcse(kcase) == 0) next

			do imat = 1, nmatcse(kcase) {

				#write (ttyout,*) 'DEBUG: creating files for case', kcase

				igroup = -1
				ig0flg = 0
				call cublineout(igroup,kcase,matcse(imat,kcase),ig0flg)

				if (ier != 0) {    # note ier is passed in common

					write (ttyout,*) 'Press return to exit'
					call crtin
					return
				}

			}
		}
	}

# write result statistics:

	# get date, time, user

	call jdatim(jda,isec)
	call todms (isec,1,ihour,imm,ss)
	call frjuld (yy,mm,dd, jda)
	write (lunresult,479) mm,dd,yy,ihour,imm,ss, uname
479	format ('Analysis complete: ',i2,'/',i2,'/',i4,' (mm/dd/yyy) ',
		i2,':',i2,':',f3.0,' UT   by user:',a)

	itotpixl = dy * dx
	write (lunresult,481) dy, dx, dz, itotpixl

481	format (/,'Results of mapping run (',i6,' lines,',i6,' samples,',
		i4,' bands)',/,30x,'(',i9,' total pixels):')
	write (lunresult,478)
478	format (/,45x,'Non-Zero          Number of',/,
		8x,'File',33x,' Depth      Non-Zero Pixels mapped',/,
		45x,'Mean Fit    Fit     Depth       F*D')
	if (nmats > 0) {   #### FUTURE: nmats includes cases, this test should not
		do igroup = 0, nzgroup {  ## output by group
			if (nmatgrp(igroup) == 0) next   # this line make nmats test
							#  above not necessary

			write (lunresult, 484) igroup
484			format (/,'***** Group:',i5,' *****')

			itotfitmappixl = 0   # total mapped non-xero pixels fit
			itotdepthmappixl = 0 # total mapped non-xero pixels depth
			itotfdmappixl = 0    # total mapped non-xero pixels f*d

			do i = 1, nmatgrp(igroup) {

				imat = matgrp(i,igroup)
				if (statsmapdepth(imat) > 0) {
					xtmp = fitmean(imat)/dble(statsmapdepth(imat))
				} else {
					xtmp = 0.0
				}

				# NOTE: only show last 44 characters of dfile

				length = lenfile(imat)
				if (igroup == 0) {

				   dfile = mfile(imat)(1:length)
				} else {

				   lengthd = lengdir(igroup)
				   dfile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length)
				}

				itmp2 = lnb(dfile)
				if (itmp2 < 45) {
					itmp2 = 44
					itmp1 = 1
				} else {
					itmp1 = itmp2 - 43
				}
				write (lunresult,540) dfile(itmp1:itmp2),xtmp,
							statsmapfit(imat),
							statsmapdepth(imat), statsmapfd(imat)
540				format (a,f6.3, 1x, i9,1x,i9,1x,i9)
				itotfitmappixl = itotfitmappixl + statsmapfit(imat)
				itotdepthmappixl = itotdepthmappixl + statsmapdepth(imat)
				itotfdmappixl = itotfdmappixl + statsmapfd(imat)
			}

			tfitpercent = (float(itotfitmappixl) / float(itotpixl))*100.0
			tdepthpercent = (float(itotdepthmappixl) / float(itotpixl))*100.0
			tfdpercent = (float(itotfdmappixl) / float(itotpixl))*100.0
			write (lunresult, 483)
483			format (80('='))
			write (lunresult, 482) igroup, itotfitmappixl,itotdepthmappixl,
						itotfdmappixl, tfitpercent,
						tdepthpercent, tfdpercent
482			format ('Group',i5,' total',35x,i9,1x,i9,1x,i9,/,
				53x,f6.2,'%',3x,f6.2,'%',3x,f6.2,'%',/)
	
		}
                #################### now do cases #######################
		do ncse = 1, nzcase {  ## output by group
			if (nmatcse(ncse) == 0) next   # this line make nmats test
							#  above not necessary

			write (lunresult, 584) ncse
584			format (/,'*****  Case:',i5,' *****')

			itotfitmappixl = 0   # total mapped non-xero pixels fit
			itotdepthmappixl = 0 # total mapped non-xero pixels depth
			itotfdmappixl = 0    # total mapped non-xero pixels f*d

			do i = 1, nmatcse(ncse) {

				imat = matcse(i,ncse)
				if (statsmapdepth(imat) > 0) {
					xtmp = fitmean(imat)/dble(statsmapdepth(imat))
				} else {
					xtmp = 0.0
				}

				# NOTE: only show last 44 characters of dfile

				length = lenfile(imat)
				lengthd = lengcdir(ncse)
				dfile = pathcase(ncse)(1:lengthd) // mfile(imat)(1:length)

				itmp2 = lnb(dfile)
				if (itmp2 < 45) {
					itmp2 = 44
					itmp1 = 1
				} else {
					itmp1 = itmp2 - 43
				}
				write (lunresult,540) dfile(itmp1:itmp2),xtmp,
							statsmapfit(imat),
							statsmapdepth(imat), statsmapfd(imat)
				itotfitmappixl = itotfitmappixl + statsmapfit(imat)
				itotdepthmappixl = itotdepthmappixl + statsmapdepth(imat)
				itotfdmappixl = itotfdmappixl + statsmapfd(imat)
			}

			tfitpercent = (float(itotfitmappixl) / float(itotpixl))*100.0
			tdepthpercent = (float(itotdepthmappixl) / float(itotpixl))*100.0
			tfdpercent = (float(itotfdmappixl) / float(itotpixl))*100.0
			write (lunresult, 483)
			write (lunresult, 582) ncse, itotfitmappixl,itotdepthmappixl,
						itotfdmappixl, tfitpercent,
						tdepthpercent, tfdpercent
582			format (' Case',i5,' total',35x,i9,1x,i9,1x,i9,/,
				53x,f6.2,'%',3x,f6.2,'%',3x,f6.2,'%',/)
	
		}
	}
	

	if (cmdverbose(-1) <= 1) {
		write (ttyout, *) 'Flushing results buffer'
	}
	call flushseqfile (lunresult, resultfile(1:lnb(resultfile)), ier)
	if (ier != 0) {
		call what (-1)
		return
	}

222	format ('Pixel coordinate: x = ',I3,' y = ',I3,/,
	' Band start range  = ',I3,'-',I3,/,
	' Band end range    = ',I3,'-',I3)
333	format ('Channel=',I3,'  Wavelen= ',F6.3,'  Ref (CR)= ',
	F6.3,'  Obs= ',F6.3,'  Obs (CR)= ',F6.3)
444	format ('Band depth     = ',F6.3,' scaled 8 bit = ',I3,/,
	'Quality of fit = ',F6.3,' scaled 8 bit = ',I3,/,
	'K-factor       = ',F6.3,/,
	'Slope          = ',F6.3,/,
	'Y-intercept    = ',F6.3,/,
	'Min chan       = ',I3,/,
	'Error message toggle (1/on, 0/off) = ',I3,/,
	'Image file flag (1/on, 0/off)      = ',I3)


# End program
	if (cmdverbose(-1) <= 1) {
		write (ttyout,*)'Updating restart file.'
	}
	icrst = 1
	call rstart (icrst)
	return
	end
