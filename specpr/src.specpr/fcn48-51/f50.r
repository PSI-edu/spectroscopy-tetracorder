	subroutine f50(ic)

#ccc  version date: 9-dec-88
#ccc  author(s): Wendy Calvin 
#ccc  language:  Ratfor
#ccc
#ccc  short description:  This routine computes a new wavelength
#ccc                   file by placing spectral absorption features
#ccc                   at the same wavelength as they occur in a 
#ccc                   reference spectrum.
#ccc                       Plots reference and working spectra,
#ccc                   marks the features in the reference spectrum,
#ccc                   from user graphics cursor in reads where
#ccc                   features are in working spectra, then
#ccc                   calculates linear wavelengths between features.
#ccc                       Finally writes new wavelengths and
#ccc                   working spectrum to file.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    
#ccc                    
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#     *******************************************************
#     * Calculates new wavelengths based on position of 
#     *  features in a reference spectrum.
#     *******************************************************

	implicit integer*4 (i-n)

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/interp"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	character*8 iname,inamw1,inamw2
	character*40 mtitl
	character*80 outline	# X-window writes
	real*4 wavset(20), chnset(20) 
# 	   y-values are in dataa
#	   old and new x-values (wavelengths) are in datac
#          reference y-values in datsc3
#	   reference x-values (waves) in datsc2

	common /cplot1/idwrf,idrf,irwrf,irrf
	common /plot1/xmax,xmin,lbnd,diff

	call eralph
	write(ttyout,10)
10	format(' Special Function f50:',/,
		'  SPECTRAL REGISTRATION',//,
	' Calculates a new wavelength file based on the position ',/,
	'  of absorption features in a reference spectrum ',/)

	if (ictrl == -1) {
		write(ttyout,20)
20		format('USAGE: file ID record # f50 (e.g. v3f50).',/,
	       	             ' Press <return> to hard EXIT.'/)
		call crtin
		ic = ihx
		return
	}

#     *** print out id, number and title ***
	write(ttyout,50) idv1,ifl1,ititl
50	format(' Registration Applied to ',a,i6,':',/,1x,a/)
	idev= idv1
	ifile= ifl1
	mtitl = ititl
	call whedr2

#     *** get wavelengths for spectra being registered ***
	write(ttyout,55)
55	format(' Enter file id and file # for wavelengths of file ',
		'to be registered ',/,
		'    or return for current wavelength file',/,
		'       or e or x to  EXIT ',/)
	call gtfl(idwfx,irwfx,il)
	nchfx = nchans
	do j=1,nchfx {
		datac(j) = data(j)
	}
	if (il == ihe || il == ihx) go to 500


#     *** get reference spectrum and reference waves ***
	write(ttyout,58)
58	format(' Enter file id and file # for reference wavelengths ',/,
		' or e or x to EXIT ',/)
	call gtfl(idwrf,irwrf,il)
	write(ttyout,59) idwrf,irwrf,ititl
59	format(' Reference Wavelengths ',a,i6,':',/,1x,a/)
	nchrf = nchans
	do j=1,nchrf {
		datsc2(j) = data(j)
	}
	if (il == ihe || il == ihx) go to 500
	write(ttyout,60)
60	format(' Enter file id and file # for reference spectrum ',/,
		' or e or x to  EXIT ',/)
	call gtfl(idrf,irrf,il)
	write(ttyout,65) idrf,irrf,ititl
65	format(' Reference Spectrum',a,i6,':',/,1x,a/)
	do j=1,nchrf {
		datsc3(j) = data(j)
	}
	if (il == ihe || il == ihx) go to 500


#     ************************************
#     * Display reference spectrum and registration spectrum
#     ************************************
68	icall = 1
	call plot2(icall,nchrf,nchfx,irtn)
	if (irtn == ihx) go to 500 		#very end
	if (irtn == ihe) go to 400		#writing of file
#
#	every thing after here is done on graphics screen (we hope)
#

#     ************************************
#     * Query, to scale reference spectrum
#     ************************************
	call serase(0,280,511,350)
	call movabs(0,330)
	call sb(0)
	write(outline,70) char(0)
	call gwrite(outline)
70	format(' Would you like to change the scale on the ',
		 'reference??  y=yes',a1)
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihe || il==ihx) go to 500
	if (il==ihy) {
		call movabs(0,310)
		call sb(0)
		write(outline,72) char(0)
		call gwrite(outline)
72		format(' Enter the scale factor ',a1)
		call crtin
		i=1
		call wjfren(i,x,il)
		sclf = x
		do j=1,nchrf {
			if (datsc3(j) == -1.23e34) next
			datsc3(j) = datsc3(j) * sclf
		   }
		call plot2(icall,nchrf,nchfx,irtn)
		if (irtn == ihx) go to 500 		#very end
		if (irtn == ihe) go to 400		#writing of file
	  }

#     ************************************
#     * Query, to window or not to window
#     ************************************
	call serase(0,280,511,350)
	call movabs(0,330)
	call sb(0)
	write(outline,74) char(0)
	call gwrite(outline)
74	format(' Would you like more than one window??  y=yes',a1)
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihe || il==ihx) go to 500
	if (il==ihy) {
		call movabs(0,310)
		call sb(0)
		write(outline,76) char(0)
		call gwrite(outline)
76		format(' Enter the number of windows ',a1)
		call crtin
		i=1
		call wjfren(i,x,il)
		nwind = x
	  } else {
		nwind = 1
	  }

	nbands = 0
	nfeat = 0
	icall = 2
	do k=1,nwind {

	call plot2(icall,nchrf,nchfx,irtn)
	if (irtn == ihx) go to 500
	if (irtn == ihe) go to 400

#     ************************************
#     * how many bands used for registration
#     ************************************
	call serase(0,280,511,350)
	call movabs(0,330)
	call sb(0)
	write(outline,75) char(0)
	call gwrite(outline)
	write(outline,1075) nbands,char(0)
	call gwrite(outline)
75	format(' Enter the number of features in this window ',
	'to be used in registration, ',a1)
1075	format(i3,' features so far, 20 features TOTAL allowed',a1)
	call crtin
	i = 1
	call wjfren (i,x,il)
	nfeat = x
	if (il==ihe || il==ihx) go to 500

#     ************************************
#     * Move graphics cursor to each absorption band in
#     *   reference spectrum, read position and mark it 
#     *   with a vertical line.
#     ************************************
	do j = nbands+1, nbands+nfeat {
		call serase (0,280,511,350)
		call movabs(0,340)
		call sb(0)
		write(outline,80) j,char(0)
		call gwrite(outline)
		write(outline,1080) char(0)
		call gwrite(outline)
		write(outline,1180) char(0)
		call gwrite(outline)
80		format(' Move cursor to center of feature # ',i3,a1)
1080		format('in the REFERENCE spectrum, then hit <rtn>',a1)
1180		format('Start at smallest wavelength and move to largest',a1)
		repeat {
			call gcrpos(ixx,iyy,xpos,ypos,
				xmax,xmin,lbnd,diff,iopcon,irtn)
			call gcchan(ixx,iyy,xpos,ypos,
				imatch,nchrf,datsc2,datsc3,irtn)
		} until (irtn != -1)
		if (irtn == ihe || irtn == ihx ) go to 500
		wavset(j) = xpos
#       *********** draw vertical line
		call movabs(ixx,46)
		call drwabs(ixx,276)

#     ************************************
#     * Move graphics cursor to corresponding band in 
#     *   spectrum being registered.  Determine channel.
#     ************************************
#		call serase(0,280,511,350)
		call movabs(0,340)
		call sb(0)
		write(outline,90) j,char(0)
		call gwrite(outline)
		write(outline,1090) char(0)
		call gwrite(outline)
90		format(' Move cursor to center of feature #',i3,a1)
1090		format('in the REGISTRATION spectrum, then hit <rtn>',a1)
		repeat {
			call gcrpos(ixx,iyy,xpos,ypos,
				xmax,xmin,lbnd,diff,iopcon,irtn)
			call gcchan(ixx,iyy,xpos,ypos,
				imatch,nchfx,datac,dataa,irtn)
		} until (irtn != -1)
		chnset(j) = imatch
	   }   #  end j=1,nfeat
	nbands = nbands + nfeat
	 }   # end k=1,nwindows

#     ************************************
#     * Using the determined wavelengths and channels
#     *   calculate linear segments for new wavelengths. 
#     ************************************
#   Extend 1st and last segments to beginning and end
#	 of the file, respectively

#	input to line gen wavset, chnset
	call lngen(nbands,nchfx,wavset,chnset,il)
	do j=1,nchfx {
		datac(j) = datsc4(j)
	}

#     ************************************
#     * Redisplay reference spectrum and registered spectrum 
#     ************************************

	call serase(0,280,511,350)
	call movabs(0,340)
	call sb(0)
	icall = 3
	call plot2(icall,nchrf,nchfx,irtn)
	if (irtn == ihx) go to 500
	if (irtn == ihb) go to 68


	call eralph
400 	continue
#     *** write history ***
	mhist = ' '
	write(ihist,420) idwrf, irwrf
420	format('f50:  new wavelengths registered against:  ',a,i6)
	write(ititl1,440) mtitl(10:10), mtitl(22:37)
440	format('New Waves for M',a,':',a16)

	return

500 	continue
	ic = ihx
	ixit = ihx
	return
	end
