	subroutine creatoutfiles (igroup, kcase, imat, ig0flg)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         creatoutfiles
#ccc  version date: 12/14/94
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: create the output files
#ccc                     for one group, one material
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
#ccc     igroup = which group is being created
#ccc     kcase  = which case is being created
#ccc     imat   = which material is being created
#ccc     ig0flg = this material is a group 0 entry.
#ccc--------------------------------------------------------------

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
	include		"../specpr/src.specpr/common/lblvol"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	character*47 envititle

	character*40 tmptitle

	integer*4 tmplength

	integer*4 igroup, kcase, imat, ig0flg, ihrecs
	integer*4 ibitspxl, lblsiz, iorg, ioutbnds
	integer*4 length, lengthd
	integer*4 cmdverbose, kmode
	integer*4 lnb, lastchar

# Base file name for material "imat"

	ier = 0

	if (imatenable(imat) == 0) {   # material not enabled, so do nt open a file

		#if (cmdverbose(-1) <= 1) write (ttyout,*) 'NOTE: material',imat,' is disabled, not creating an output file'

		tmptitle = otitle(imat)(1:40)
		tmplength = lnb(otitle(imat)(1:40))

		# group 0 will be written out for each igroup, so limit to 1st one
		if ( group(imat) == 0 & igroup < 2) {
			write (ttyout, 101) imat, group(imat), tmptitle(1:tmplength), igroup
			# write (ttyout,*) "        (grp=0)    DISABLED      otitle ", otitle(imat)
			# write (ttyout,*) "        (grp=0)    DISABLED      case   ", icase(imat)
			if (icase(imat) > 0) write (ttyout,*) "                      icase=", icase(imat) 

		} else if ( group(imat) > 0 ) {
			write (ttyout, 101) imat, group(imat), tmptitle(1:tmplength), igroup
			# write (ttyout,*) "        (grp>0)    DISABLED      otitle ", otitle(imat)
			# write (ttyout,*) "        (grp>0)    DISABLED      case   ", icase(imat)
			if (icase(imat) > 0) write (ttyout,*) "                      icase=", icase(imat) 
		}
101		format ('   Material',i6, ' group ', i4,' DISABLED otitle= ', a, '     not creating an output file',
			'  igroup=', i4)

		return
	}

180	ibitspxl = obits(imat)  # output file bits per pixel
	if (ibitspxl == 16) {
		oreclen = dx*2  # output record length (I*2, so = dx*2 pixels)
	} else {
		oreclen = dx  # output record length (bytes, so = dx pixels)
	}

	# figure out output label size (NOTE IF cublineout.r subroutine
        #    is changed, so must this one!!!)

	lblsiz= oreclen  # output file size is record length
	if (lblsiz < 299) {
		lblsiz = oreclen*(int(299/oreclen)+1)
	}
	ihrecs = lblsiz/oreclen
	if (ihrecs < 1) ihrecs = 1
	iorg  = 1        # out vicar file organization is BIL
	ioutbnds = 1     # one band image for each output

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) 'Opening files, record length =', oreclen
	}

	length = lenfile(imat)
	if (igroup > -1) {
		kmode = 0
		lengthd = lengdir(igroup)
	} else if (kcase > 0) {
		kmode = 1
		lengthd = lengcdir(kcase)
	} else {
		write (ttyout,*) 'ERROR creating files'
		write (ttyout,*) 'group ', igroup, ' case ',kcase,' incompatible'
		call what (-1)
		return
	}

	if (kmode == 0) {
		dfile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.depth'
	} else {
		dfile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.depth'
	}
	open (unit=lund, file=dfile(1:lengthd+length+6), access='direct',
		recl=lblsiz,
		form='unformatted', status='new', iostat=ier)

	if (ier != 0) {
		write (ttyout,187) ier, 'depth', dfile(1:lengthd+length+6)
187		format (' OPEN ERROR',i5,' on ',a,' file:',/,a)
		call what (-1)
		return
	}

# now write a VICAR header to the file

	call makvicarlabel(chbuff(1:lblsiz),lblsiz,ibitspxl,oreclen,
		iorg,dy,dx,ioutbnds,otitle(imat)//' DEPTHS','tetracorder',ier)

	write(lund,rec=1,iostat=ier) chbuff(1:lblsiz)
	if (ier != 0) {
		write (ttyout,188) ier, 'depth', dfile(1:lengthd+length+6)
188		format (' VICAR header write ERROR',i5,' on ',a,' file:',/,a)
		call what (-1)
		return
	}

	close (lund, iostat=ier)
	if (ier != 0) {
		write (ttyout,189) ier, 'depth', dfile(1:lengthd+length+6)
189		format (' CLOSE ERROR',i5,' on ',a,' file:',/,a)
		call what (-1)
		return
	}

# now write an ENVI header file

       lastchar = lnb(otitle(imat))
       envititle = otitle(imat)(1:lastchar) // 'DEPTHS'
       call wrtenvihdr(lund, dfile(1:lengthd+length+6),
            envititle, ioutbnds)
#           otitle(imat)(1:lastchar) // ' DEPTHS', ioutbnds)

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) 'New file ',
			dfile(1:lengthd+length+6),' CREATED'
	}

	if (kmode == 0) {
		ffile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.fit'
	} else {
		ffile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.fit'
	}
	open (unit=lunf, file=ffile(1:lengthd+length+4),
		access='direct',recl=lblsiz,
              form='unformatted', status='new', iostat=ier)

	if (ier != 0) {
		write (ttyout,187) ier, 'fit', ffile(1:lengthd+length+4)
		call what (-1)
		return
	}

# now write a VICAR header to the file

	call makvicarlabel(chbuff(1:lblsiz),lblsiz,ibitspxl,oreclen,
		iorg,dy,dx,ioutbnds, otitle(imat)//' FITS','tetracorder',ier)

	write(lunf,rec=1,iostat=ier) chbuff(1:lblsiz)
	if (ier != 0) {
		write (ttyout,188) ier, 'fit', ffile(1:lengthd+length+4)
		call what (-1)
		return
	}

# now write an ENVI header file

       lastchar = lnb(otitle(imat))
       call wrtenvihdr(lunf, ffile(1:lengthd+length+4),
           otitle(imat)(1:lastchar) // ' FITS', ioutbnds)

	close (lunf, iostat=ier)
	if (ier != 0) {
		write (ttyout,189) ier, 'fit', ffile(1:lengthd+length+4)
		call what (-1)
		return
	}
	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) 'New file ',
			ffile(1:lengthd+length+4),' CREATED'
	}

	if (kmode == 0) {
		fdfile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.fd'
	} else {
		fdfile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.fd'
	}
	open (unit=lunfd, file=fdfile(1:lengthd+length+3),
		access='direct',recl=lblsiz,
              form='unformatted', status='new', iostat=ier)

	if (ier != 0) {
		write (ttyout,187) ier, 'fit*depth', fdfile(1:lengthd+length+3)
		call what (-1)
		return
	}

# now write a VICAR header to the file

	call makvicarlabel(chbuff(1:lblsiz),lblsiz,ibitspxl,oreclen,
		iorg,dy,dx,ioutbnds, otitle(imat)//' F*D','tetracorder',ier)

	write(lunfd,rec=1,iostat=ier) chbuff(1:lblsiz)
	if (ier != 0) {
		write (ttyout,188) ier, 'fit*depth', fdfile(1:lengthd+length+3)
		call what (-1)
		return
	}

	close (lunfd, iostat=ier)
	if (ier != 0) {
		write (ttyout,189) ier, 'fit*depth', fdfile(1:lengthd+length+3)
		call what (-1)
		return
	}

# now write an ENVI header file

       lastchar = lnb(otitle(imat))
       call wrtenvihdr(lunfd, fdfile(1:lengthd+length+3),
           otitle(imat)(1:lastchar) // ' F*D', ioutbnds)

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) 'New file ',
			fdfile(1:lengthd+length+3),' CREATED'

		write (ttyout,*) ' '
		write (ttyout,*) ' '
	} else if (cmdverbose(-1) == 2) {
		write (ttyout,*) 'CREATED FILE SET:',
					dfile(1:lengthd+length)
	}

	return
	end
