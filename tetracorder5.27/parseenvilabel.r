	subroutine parseenvilabel (qlun,icube,samples,lines,bands,
		header_offset,data_type,interleave,byte_order,ier)

	implicit integer*4 (i-n)

#ccc  name:  parseenvilabel
#ccc  version date: 2/19/10
#ccc  author(s): K.Eric Livo
#ccc  language: ratfor
#ccc
#ccc  short description: parse ENVI label to get parameters needed
#ccc
#ccc  algorithm description: search strings in the ENVI label for
#ccc                         various fields
#ccc  system requirements: none
#ccc  subroutines called:       wjfren
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc
#ccc
#ccc	ENVI Header File Requirements
#ccc	'ENVI' must comprise the first 4 characters of the header file
#ccc	text records must be 120 characters or less
#ccc		(charge assignment line:   'character*120 ch')
#ccc	one keyword with value per line (text record)
#ccc	keywords may be in any order
#ccc	keywords may start anywhere on line
#ccc		(with preceeding white space if desired)
#ccc	values must follow keyword with one or more white spaces and/or
#ccc	 equals sign ('=')
#ccc	value strings must be 80 characters or less (within the record)
#ccc
#ccc
#     keywords defined
#
#        interleave   = : (ENVI)
#		'bil' (filorg=1)
#		'bip' (filorg=2)
#		'bsq' (filorg=3)
#		default 'BNK'

#        data_type    = : (ENVI)
#		[NOT Implemented] 1 = Byte
#				8bit unsigned integer (0 to 255)
#		2 (numtyp=1) [or dattyp or filtype(9,x)]; Integer (Int*2)
#				16bit signed integer  (-32768 to +32767)
#		3 (numtyp=2) [or dattyp or filtype(9,x)]; Long Integer  (Int*4)
#				32bit signed integer (~ +/- 2 billion)
#		4 (numtyp=3) [or dattyp or filtype(9,x)]; Floating-point  (Real*4)
#				32bit real (+/- 1e38)
#		default -1

#        byte_order   = : (ENVI)
#		0 = Intel (PC)	[LOWHI]
#		1 = IEEE		[HILOW]
#		default -1
#ccc


# Note: Windows PCs need ".h" at end of include file (or use quotes)
#        include '../specpr/src.specpr/common/label1.h'
	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

        include '../specpr/src.specpr/common/lbl4'
        include '../specpr/src.specpr/common/lundefs'

#	change record parsing length by resizing 'ch'
#	character*1536 ch
	character*120 ch
	character*(*) icube
	character*3 interleave

	integer*4 qlun,samples,lines,bands
	integer*4 header_offset,data_type,byte_order

	character*1 iquote
	integer*4 i, ii, j, jj, istart, iend, icmdmax
	integer*4 nkeywords, textlines_max
	integer*4 istchr, st_target
	integer*4 ilen_ch, ilen_iopcon
	real*4 rvalue

	character*1 lowch,highch,cnull

#	integer*4 ttyout
#
###### TEMPORARY:
#	ttyout = 6
#
# initialize variables
#
	iquote = char(39)  # a single quote character
	istchr = 1
	st_target = 10

	samples = 0
	lines = 0

	lowch = char(32)     # lowest character (lower ones are control ch)
	highch = char(126)   # highest character (we exclude 8-bit, and DEL)
	cnull = char(0)      # null character
	#  iopcon declared as 80 characters in 'lbl4.h'
	ilen_iopcon = len(iopcon)
	ilen_ch = len(ch)

#  CHANGE to DO WHILE not EOF
	textlines_max = 20000
	do nkeywords = 1, textlines_max {

# 	file icube was opened in calling subroutine .tetracorder/opencube

		read (qlun,'(a)',end=999,iostat=ier) ch
		if (ier != 0) {
			write (ttyout,*)'ERROR on read in opencube: ',ier
			call what(-1)
			return
		}

#		check that all characters are in range, if not, set to blank.
		do i = 1, ilen_ch {   # this starts the "check do loop"
			if (ch(i:i) == cnull && i < ilen_ch) {
#				blank out rest of label after find first null.
				do jj = i, ilen_ch {
					ch(jj:jj) = ' '
				}
				# this breaks out of the "check do loop"
				   # (NOTE: specpr version ratfor fix:
				   # don't put comment after the break [break-comment line is bad])
				   # possibly associated with end do loop as well
				break
			}
			if (ch(i:i) < lowch || ch(i:i) > highch) {
				ch(i:i) = ' '  # set to blank
			}
		}
		write(*,*) nkeywords, ch

#		find and parse labels for values
		istart = ifnb(ch(1:ilen_ch))

		if (ch(istart:istart + 6) == 'samples') {
			istart = istart + 7
			call header_parse_number(ch,istart,rvalue)
			samples = int(rvalue)
		}

		if (ch(istart:istart + 4) == 'lines') {
			istart = istart + 5
			call header_parse_number(ch,istart,rvalue)
			lines = int(rvalue)
		}

		if (ch(istart:istart + 4) == 'bands') {
			istart = istart + 5
			call header_parse_number(ch,istart,rvalue)
			bands = int(rvalue)
		}

		if (ch(istart:istart + 12) == 'header offset') {
			istart = istart + 13
			call header_parse_number(ch,istart,rvalue)
			header_offset = int(rvalue)
		}

		if (ch(istart:istart + 8) == 'data type') {
			istart = istart + 9
			call header_parse_number(ch,istart,rvalue)
			data_type = int(rvalue)
		}

		if (ch(istart:istart + 9) == 'interleave') {
			j = istart + 12
			istart = j + ifnb(ch(j+1:ilen_ch))
			#  interleave values: 'bil', 'bip', 'bsq'
			interleave = ch(istart:istart + 2)
			write (ttyout,*) 'parseenviheader: interleave= ', interleave
		}

		if (ch(istart:istart + 9) == 'byte order') {
			istart = istart + 10
			call header_parse_number(ch,istart,rvalue)
			byte_order = int(rvalue)
		}

	}
#
#
999	return
	end

#         if(iopcon2(1:4)=='ivfl') {
#            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
#            i = istart + lnb(iopcon2(istart:4 + icmdmax))
#            ivfl = iopcon2(istart:i) }

#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine header_parse_number(ch,istart,rvalue)

	implicit integer*4 (i-n)

#ccc  name:  header_parse_number
#ccc  version date: 2/19/10
#ccc  author(s): K.Eric Livo
#ccc  language: ratfor
#ccc
#ccc  short description: parse header record (text) for value
#ccc
#ccc

# Note: Windows PCs need ".h" at end of include file (or use quotes)
#        include '../specpr/src.specpr/common/label1.h'
        include '../specpr/src.specpr/common/lbl4'
        include '../specpr/src.specpr/common/lundefs'

	character*(*) ch
	integer*4 istart
	real*4 rvalue

	integer*4 ilen_ch, ilen_iopcon, iend, j, il
	real*4 x

#	integer*4 ttyout
#
###### TEMPORARY:
#	ttyout = 6

	#  iopcon declared as 80 characters in 'lbl4.h'
	ilen_iopcon = len(iopcon)
	ilen_ch = len(ch)

	#  istart is the first char position to parse for value
	istart = istart -1 + ifnb(ch(istart:ilen_ch))
	iend = istart + ilen_iopcon -1
	if (iend > ilen_ch) {
		iend = ilen_ch
	}
	iopcon=ch(istart:iend)

	j = 1
	call wjfren(j,x,il)
#DEBUG	write(*,*) istart, j, x, il
	if (il == 0 && x > 0) {
		rvalue = x
	} else {
		# check for zero numeric value
		if((j < ilen_ch + 1) && il == 0) {
			rvalue = x
		} else {
			write (ttyout,10)
10			format ('ERROR decoding value of header record')
			call what(j)
			ier = 1
			return
		}
	}

	return
	end
