	subroutine cublineout (igroup, kcase, imat, ig0flg)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         cublineout
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: outputs the results from cubecorder
#ccc                     for one group
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
#ccc     igroup = which group is being output
#ccc     kcase  = case
#ccc     imat   = which material is being output
#ccc     ig0flg = this material is a group 0 entry.
#ccc              so do special checks and see if output
#ccc              should be zeroed because a different material
#ccc              was best.
#ccc     yel    = line number (last line = dy in tricube.h)
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

# arrays for buffering output

	include "obuffers.h"

	integer*4 igroup, kcase, imat, ig0flg, ihrecs
	integer*4 length, lengthd, lblsiz
	integer*4 i4pack4
# RED 07/07/2008 Added declaration of i since no longer global in tricube.h
	integer*4 kmode,jj,jrec, jrec0, j, ij, i
	integer*4 iallzd,iallzf,iallzfd

	character*200 chzip

	IER = 0

	if (imatenable(imat) == 0) {   # material not enabled, so do nt open a file

		#if (cmdverbose(-1) <= 1) write (ttyout,*) 'NOTE: material',imat,' is disabled, cublineout: no output line '
		return
	}

	if (imat < 0) {
		write (ttyout,*) 'ERROR: cublinout: material < 0: imat=',imat
		return
	}
	#write (ttyout,*) 'DEBUG: cubelinout imat=',imat,' line=',yel,' igroup=',
	#				igroup,' ig0flg=',ig0flg,' caseflg=',kcase
	#if (kcase == 1) write (ttyout,*) 'DEBUG: imat=',imat,' case=',kcase,
	#				' ig0flg=',ig0flg,' igroup=',igroup

	if (olinb(imat) < 0 | olinb(imat) > bufferlines) {   #ERROR

		write (ttyout,*) 'OUTPUT LINE BUFFER ERROR'
		write (ttyout,*) 'OUTPUT LINE skipped'
		return
	}

	olinb(imat) = olinb(imat) + 1    # output buffer line number

	#if (imat == 9) {
	#	write (ttyout,*) 'DEBUG: cubelinout imat=',imat,' olinb=',
	#				olinb(imat)
	#}
	#if (kcase == 1) write (ttyout,*) 'DEBUG: imat=',imat,' case=',
	#				kcase,' ig0flg=',ig0flg,' igroup=',igroup

	iallzd  = 0  # depth all zero counter for line
	iallzf  = 0  # fit   all zero counter for line
	iallzfd = 0  # f*d   all zero counter for line

	do i = 1,dx  {       # prepare sheet analysis for output

	   #if (kcase == 1) write (ttyout,*) 'DEBUG: imat=',imat,' case=',kcase,' i=',i

	   if (ig0flg == 1) {  # this is a group 0 material

		if (igbest(igroup,i) == imat) { # this group 0 material is
						# the best match for the group

		   i4buff(i) = nint(odepth(imat,i)*bdscal(imat)) # depth
		   i4buf2(i) = nint(ofit(imat,i)*qfscal(imat))   # fit
		   i4buf3(i) = nint(ofd(imat,i)*bdscal(imat)) # f*d

		} else {

		   i4buff(i) = 0
		   i4buf2(i) = 0
		   i4buf3(i) = 0

		}

	   } else {
		   i4buff(i) = nint(odepth(imat,i)*bdscal(imat)) # depth
		   i4buf2(i) = nint(ofit(imat,i)*qfscal(imat))   # fit
		   i4buf3(i) = nint(ofd(imat,i)*bdscal(imat)) # f*d

	   }

	   if (obits(imat) == 8) {   # byte/pixel output
		# Check for values outside of range ( < 0 or > 255)

		if (i4buff(i) <= 0) {
			i4buff(i) = 0  # depth
			iallzd  = iallzd  + 1  # depth all zero
		}
		if (i4buf2(i) <= 0) {
			i4buf2(i) = 0  # fit
			iallzf  = iallzf  + 1  # fit all zero
		}
		if (i4buf3(i) <= 0) {
			i4buf3(i) = 0  # f*d
			iallzfd = iallzfd + 1  # f*d all zero
		}

		if (i4buff(i) > 255) i4buff(i) = 255  # depth
		if (i4buf2(i) > 255) i4buf2(i) = 255  # fit
		if (i4buf3(i) > 255) i4buf3(i) = 255  # f*d

	   } else if (obits(imat) == 16) {   # byte/pixel output

		# Check for values outside of range ( < -32767 or > 32767)

		if (i4buff(i) < -32767) i4buff(i) = -32767  # depth
		if (i4buf2(i) < -32767) i4buf2(i) = -32767  # fit
		if (i4buf3(i) < -32767) i4buf3(i) = -32767  # f*d

		if (i4buff(i) > 32767)  i4buff(i) =  32767  # depth
		if (i4buf2(i) > 32767)  i4buf2(i) =  32767  # fit
		if (i4buf3(i) > 32767)  i4buf3(i) =  32767  # f*d

		if (i4buff(i) == 0) {
			iallzd  = iallzd  + 1  # depth all zero
		}
		if (i4buf2(i) == 0) {
			iallzf  = iallzf  + 1  # fit all zero
		}
		if (i4buf3(i) == 0) {
			iallzfd = iallzfd + 1  # f*d all zero
		}
	   }

	   # put in buffer (when buffer full, write it out.)

	   if (ig0flg != 1) {  # this is a NOT group 0 material
                                            # so buffer it. (group 0 not buffered)

		ofitbuff(imat,olinb(imat),i)   = i4buf2(i) # fit
		odepthbuff(imat,olinb(imat),i) = i4buff(i) # depth
		ofdbuff(imat,olinb(imat),i)    = i4buf3(i) # f*d

		#if (imat != 9 & i == 1) {
		#	write (ttyout,*) 'DEBUG: putting results in buffer line',
		#				olinb(imat),' imat=',imat
		#}
	   }
	   
	}

	# now check if the sheet for material imat is all zero.  If so,
	# mark it, and return.  (Group 0 not checked.)  To be qualify, we
	# must not already be buffering a previous line (so olinb(imat)
	# must still be 1), and we must not be at the end of the file (so
	# yel < dy, dy = # lines in file).  All depth, fit, and f*d data
	# must be zero for all pixels on the line (dx = # pixels on line).

	if ((ig0flg != 1) & olinb(imat) == 1 & iallzd == dx &
		   iallzf == dx & iallzfd == dx & yel < dy) {

		if (izeroline(imat) == 0) izeroline(imat) = yel # begin line with all zeros
#RED
#		nzeroline = yel
		nzeroline(imat) = yel                           # current last zero line
		olinb(imat) = olinb(imat) - 1                   # do not  buffer this line

                #DEBUG:
		#write(ttyout,*) 'DEBUG: imat ',imat,
		#		'  line ',yel,' output data are all zero'

		return
	}
	# else {
	#	#DEBUG:
	#	write (ttyout,*) 'DEBUG: imat ',imat,
	#			'  line ',yel,' #zero pixels=',iallzd,iallzf,iallzfd
	#}

	if (olinb(imat) < bufferlines & yel < dy & ig0flg != 1) {
		#if (imat != -999) write (ttyout,*) 'DEBUG: buffer not full yet, imat=',imat
                # buffer not full yet
		return
	}
	
	#if (imat != 9) write (ttyout,*) 'DEBUG: buffer now full, imat=',imat,
	#					' olinb=',olinb(imat)

      do jj = 1, olinb(imat) {   # begin buffer dumping loop

	   if (ig0flg != 1) {  # this is a NOT group 0 material
                               # so get from buffer. (group 0 not buffered)

		do i = 1, dx {   # restore buffer to i4buf arrays
				# because they are equivelanced to character arrays
				# to output byte data.

		   i4buf2(i) = ofitbuff(imat,jj,i)    # fit
		   i4buff(i) = odepthbuff(imat,jj,i)  # depth
		   i4buf3(i) = ofdbuff(imat,jj,i)     # f*d

		}
	   }
	#   if (imat != 9) {
	#	write (ttyout,*) 'DEBUG: writing buffer line', jj,
	#			' material=',imat
	#   }

	if (obits(imat) == 8) {   # byte/pixel output

	    oreclen = dx   # output record length (bytes, so = dx pixels)

	    # Pack band depths to 1 byte integers

	    paksiz = 8

	} else if (obits(imat) == 16) {

	    oreclen = dx*2  # output record length (I*2, so = dx*2 pixels)

	    # Pack band depths to 2 byte integers

	    paksiz = 16

	} else {

		write (ttyout, *) 'ERROR: output must be 8 or 16 bits'
		call what (-1)
		return

	}
	    ierr = i4pack4 (i4buff,dx,paksiz)

	    if (ierr != 0) {  # Check for error in packing
		write (ttyout,*)
		write (ttyout,*)'ERROR: packing depths to 8 bit numbers'
		write (ttyout,*)
		call what (-1)
		return
	    }

	    ierr = i4pack4 (i4buf2,dx,paksiz)

	    if (ierr != 0) {  # Check for error in packing
		write (ttyout,*)
		write (ttyout,*)'ERROR: packing fits to 8 bit numbers'
		write (ttyout,*)
		call what (-1)
		return
	    }

	    ierr = i4pack4 (i4buf3,dx,paksiz)

	    if (ierr != 0) {  # Check for error in packing
		write (ttyout,*)
		write (ttyout,*)'ERROR: packing f*d to 8 bit numbers'
		write (ttyout,*)
		call what (-1)
		return
	    }

	if (izeroline(imat) > 0 ) {   # we have lines of zeros to output

	    do j = 1, dx {
		i4buf0(j) = 0
	    }

	    ierr = i4pack4 (i4buf0,dx,paksiz)

	    if (ierr != 0) {  # Check for error in packing
		write (ttyout,*)
		write (ttyout,*)'ERROR: packing f*d to 8 bit numbers'
		write (ttyout,*)
		call what (-1)
		return
	    }

	}

	# figure out output label size (NOTE IF creatoutfiles.r 
	#    subroutine is changed, so must this one!!!)

	lblsiz= oreclen  # output file size is record length
	if (lblsiz < 299) {
		lblsiz = oreclen*(int(299/oreclen)+1)
	}
	ihrecs = lblsiz/oreclen
	if (ihrecs < 1) ihrecs = 1

        # Compute the record number = current line number
        #                                  - number of lines in buffer
        #                                  + buffer line number + header records

	jrec = yel - olinb(imat) + jj + ihrecs

	#if (imat < 5) write (ttyout,*) 'DEBUG: imat=',imat,
	#			' olinb=',olinb(imat),' yel=',yel,
	#			' jrec=',jrec,' jj=',jj,
	#			' grp=',igroup,' case=',kcase,' ig0flg=',ig0flg

	if (jrec < 1 | jrec < ihrecs) {
		write (ttyout,*) 'I/O ERROR: record number less than zero:',jrec
		write (ttyout,*) 'imat=',imat,' line=',yel,
				' olinb=',olinb(imat),' jj=',jj
		write (ttyout,*) ' '
		return
	}

	# Write depths to output file

	if (jj == 1) {  # 1st line of buffer output, so open file

		# Write band depths to output file

		length= lenfile(imat)
		if (igroup > -1) {
			kmode = 0
			lengthd = lengdir(igroup)
		} else if (kcase > 0) {
			kmode = 1
			lengthd = lengcdir(kcase)
		} else {
			write (ttyout,*) 'ERROR writing files'
			write (ttyout,*) 'group ', igroup, ' case ',
					kcase,' incompatible'
			call what (-1)
			return
		}
		if (kmode == 0) {
			dfile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.depth'
		} else {
			dfile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.depth'
		}

		open (unit=lund, file=dfile(1:lengthd+length+6),
			access='direct',recl=oreclen,
			form='unformatted', status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'depth', 
					dfile(1:lengthd+length+6)
			call what (-1)
			return
		}
	}

	if (izeroline(imat) > 0 ) {   # we have lines of zeros to output
		do ij = izeroline(imat), nzeroline(imat) {

			jrec0 = ij + ihrecs
			write(lund,rec=jrec0,iostat=ier) chbuf0(1:oreclen)

			if (ier != 0) {   # Check for error in writing

				write (ttyout,288) ier, 'depth', 
						dfile(1:lengthd+length+6), ij
				call what (-1)
				return
			}
		}
	}

	write(lund,rec=jrec,iostat=ier) chbuff(1:oreclen)

	if (ier != 0) {   # Check for error in writing

		write (ttyout,288) ier, 'depth', 
				dfile(1:lengthd+length+6), jrec
288			format (' VICAR write ERROR',i5,
					' on ',a,' file:',/,a,
				/,'    I/O error at record', i6)
		call what (-1)
		return
	}

	if (jj == olinb(imat)) {  # last line of buffer output, so close file

		close (lund, iostat=ier)
		if (ier != 0) {
			write (ttyout,189) ier, 'depth',
						dfile(1:lengthd+length+6)
			call what (-1)
			return
		}

		if (ocompress(imat) == 1 & yel == dy) {  # compress output

			chzip = 'gzip ' // dfile // char(0)
			write (ttyout,*) 'compressing: gzip ', dfile, ' line ',yel
			call system (chzip)
		}
	}

	# Write fits to output file

	if (jj == 1) {  # 1st line of buffer output, so open file

		if (kmode == 0) {
			ffile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.fit'
		} else {
			ffile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.fit'
		}
		open (unit=lunf, file=ffile(1:lengthd+length+4),
			access='direct',recl=oreclen,
			form='unformatted', status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'fit', ffile(1:lengthd+length+4)
			call what (-1)
			return
		}

	}

	if (izeroline(imat) > 0 ) {   # we have lines of zeros to output
		do ij = izeroline(imat), nzeroline(imat) {

			jrec0 = ij + ihrecs
			write(lunf,rec=jrec0,iostat=ier) chbuf0(1:oreclen)

			if (ier != 0) {   # Check for error in writing

				write (ttyout,288) ier, 'fit', 
						ffile(1:lengthd+length+4), ij
				call what (-1)
				return
			}
		}
	}

	write(lunf,rec=jrec,iostat=ier) chbuf2(1:oreclen)

	if (ier != 0) {   # Check for error in writing

		write (ttyout,288) ier, 'fit',
				ffile(1:lengthd+length+4), jrec
		call what (-1)
		return
	}

	if (jj == olinb(imat)) {  # last line of buffer output, so close file
		close (lunf, iostat=ier)
		if (ier != 0) {
			write (ttyout,189) ier, 'fit', ffile(1:lengthd+length+4)
			call what (-1)
			return
		}
		if (ocompress(imat) == 1 & yel == dy) {  # compress output

			chzip = 'gzip ' // ffile // char(0)
			write (ttyout,*) 'compressing: gzip ', ffile, ' line ',yel
			call system (chzip)
		}

	}

	# write fit*depth file

	if (jj == 1) {  # 1st line of buffer output, so open file

		if (kmode == 0) {
			fdfile = pathgrp(igroup)(1:lengthd) // mfile(imat)(1:length) // '.fd'
		} else {
			fdfile = pathcase(kcase)(1:lengthd) // mfile(imat)(1:length) // '.fd'
		}
		open (unit=lunfd, file=fdfile(1:lengthd+length+3),
			access='direct',recl=oreclen,
			form='unformatted', status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'f*d',fdfile(1:lengthd+length+3)
			call what (-1)
			return
		}
	}

	if (izeroline(imat) > 0 ) {   # we have lines of zeros to output
		do ij = izeroline(imat), nzeroline(imat) {

			jrec0 = ij + ihrecs
			write(lunfd,rec=jrec0,iostat=ier) chbuf0(1:oreclen)

			if (ier != 0) {   # Check for error in writing

				write (ttyout,288) ier, 'f*d', 
						fdfile(1:lengthd+length+3), ij
				call what (-1)
				return
			}
		}
		# last output in this block, so now zero zeroline start and end

		izeroline(imat) = 0
		nzeroline(imat) = 0
	}

	write(lunfd,rec=jrec,iostat=ier) chbuf3(1:oreclen)

	if (ier != 0) {   # Check for error in writing

		write (ttyout,288) ier, 'f*d',
				fdfile(1:lengthd+length+3), jrec
		call what (-1)
		return
	}

	if (jj == olinb(imat)) {  # last line of buffer output, so close file
		close (lunfd, iostat=ier)
		if (ier != 0) {
			write (ttyout,189) ier, 'f*d',fdfile(1:lengthd+length+3)
			call what (-1)
			return
		}
		if (ocompress(imat) == 1 & yel == dy) {  # compress output

			chzip = 'gzip ' // fdfile // char(0)
			write (ttyout,*) 'compressing: gzip ', fdfile, ' line ',yel
			call system (chzip)
		}
	}

      }   # end of buffer dumping loop

      olinb(imat) = 0  # reset buffer lines

	#write (ttyout,*) 'DEBUG: image output complete'

187	format (' OPEN ERROR',i5,' on ',a,' file:',/,a)
189	format (' CLOSE ERROR',i5,' on ',a,' file:',/,a)

	return
	end
