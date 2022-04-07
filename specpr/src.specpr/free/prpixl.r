	subroutine prpixl (ipx, ipxend, igo)
	implicit integer*4 (i-n)

#ccc  version date: %W% %G% %U%
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine processes the pixel coordinate on the
#ccc	command line in the form of px(n1,n2,n3) and puts the coordinates in
#ccc	the filreq array (file request array) and changes the input line
#ccc	"px(n1,n2,n3)" to the last record to be read so the rest of specpr
#ccc    can handle it without errors.  It also handles blocks in
#ccc    the for "px(n1+m1,n2+m2,*)" or permutations of the *
#ccc  algorithm description: To initiate its use the user
#ccc          types in a device specifier, the characters px, and 3
#ccc          coordinates enclosed in parentheses and separated by commas.
#ccc          As an example, the command line
#ccc                                  wpx(3,5,*)
#ccc          indicates the device specified by the letter w, and that the user
#ccc          wants to extract the spectrum of the pixel located at y=3, x=5.
#ccc          The asterisk flags the z dimension in a 3-d data file as the 
#ccc          extraction direction.  A space between the 'w' and 'px' is
#ccc          allowed.  This routine parses and interprets these types of 
#ccc          command lines and calls the appropriate extraction routines.
#ccc          In addition, block extraction (resulting in a tty plot of the
#ccc          average of this block) can also be initiated by a command
#ccc          line of the form
#ccc                            wpx(3+2,5+4,*).
#ccc          This subroutine parses and interprets this type of command
#ccc          line as well.
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          wjfren, what, devlun, lstrec
#ccc  argument list description: where to go to when return: igo
#ccc                             end of pixel coords in iopcon: ipxend
#ccc                             where to start looking in iopcon: ipx
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#   
	include "../common/lbl4"
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/ioftyp"
	include "../common/iocontrol"
#RED
	integer*4 ihchar      # function ihchar

	ihstar = ihchar('*')
	ihcoma = ihchar(',')
	ihrpar = ihchar(')')
	ihplus = ihchar('+')
#
# default box size is zero
#
	fiobox(1) = 0
	fiobox(2) = 0
	fiobox(3) = 0

	ipxend = ipx
#
# pixel processing done on 3d files
#
	iipx = ipx+3
	call wjfren (iipx,x,il)           # get 1st coordinate
	if (il == ihstar) {               # skewer this dimension
		filreq(1) = -1
		fiobox(1) = -1
	} else if (il == 0 || il == ihcoma || il == ihplus) {
		if (x > 0) {
			filreq(1) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	} else {
		call what(iipx)            # error
		write (ttyout,5) iprom
		go to 1000
	}
	if (il == 0) {
		call wjfren (iipx, x, il)    # there was a blank after
						#   the last number
		if (x != 0) {                # shouldn't get a number yet
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il == ihplus) {                             # box is being requested
		call wjfren (iipx, x, il)               # get box size
		if (x > 0 && (il == 0 || il == ihcoma)) {
			fiobox(1) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il != ihcoma) {   #comma should be next
		call wjfren (iipx,x,il)
		if (il != ihcoma) {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	call wjfren (iipx,x,il)  # get 2nd coordinate
	if (il == ihstar) {
		filreq(2) = -1
		fiobox(2) = -1
		if (filreq(1) == -1) {       # already got a *
			call what (iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	} else if (il == 0 || il == ihcoma || il == ihplus) {
		if (x > 0) {
			filreq(2) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	} else {
		call what(iipx)            # error
		write (ttyout,5) iprom
		go to 1000
	}
	if (il == 0) {
		call wjfren (iipx, x, il)    # there was a blank after
						#   the last number
		if (x != 0) {                # shouldn't get a number yet
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il == ihplus) {                             # box is being requested
		call wjfren (iipx, x, il)               # get box size
		if (x > 0 && (il == 0 || il == ihcoma)) {
			fiobox(2) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il != ihcoma) {   #comma should be next
		call wjfren (iipx,x,il)
		if (il != ihcoma) {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	call wjfren (iipx,x,il)  # get 3rd coordinate
	if (il == ihstar) {
		filreq(3) = -1
		fiobox(3) = -1
		if (filreq(1) == -1 || filreq(2) == -1) {  # already got *
			call what (iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	} else if (il == 0 || il == ihrpar || il == ihplus) {
		if (x > 0) {
			filreq(3) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	} else {
		call what(iipx)            # error
		write (ttyout,5) iprom
		go to 1000
	}
	if (il == 0) {
		call wjfren (iipx, x, il)    # there was a blank after
						#   the last number
		if (x != 0) {                # shouldn't get a number yet
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il == ihplus) {                             # box is being requested
		call wjfren (iipx, x, il)               # get box size
		if (x > 0 && (il == 0 || il == ihrpar)) {
			fiobox(3) = x+0.5
		} else {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
	if (il != ihrpar) {   #right paren should be next
		call wjfren (iipx,x,il)
		if (il != ihrpar) {
			call what(iipx)            # error
			write (ttyout,5) iprom
			go to 1000
		}
	}
#
# set end of pixel coords
#
	ipxend = iipx - 1

# find device and decode last record number to read

	ifound = 0
	if (ipx > 1) {
		do i = ipx-1, 1, -1 {
			if (iopcon(i:i) == 'v' ||
				iopcon(i:i) == 'w' ||
				iopcon(i:i) == 'd' ||
				iopcon(i:i) == 'u' ||
					iopcon(i:i) == 'y') {
#						convert letter to logical unit
				call devlun(0,ihchar(iopcon(i:i)),lunp)
#						determine last record to read
				call lstrec (lunp,lrec)
#						put number into character string
				if (ioutverbose == 0) {
					write (ttyout,"(' lrec=',i12)") lrec
				}
				if (ipxend > ipx+5) {
					write (iopcon(ipx:ipxend),200) lrec
					do ik = ipx, ipxend {
						if (iopcon(ik:ik) == char(0)) {
							iopcon(ik:ik) = ' '
						}
					}
					ifound = 1
					go to 300
				} else {
					write (ttyout,201)
					iopcon(ipx:ipx) = '1'
					do ik = ipx+1, iipx-1 {
						iopcon(ik:ik) = ' '
					}
					go to 300
				}
			} else if (iopcon(i:i) == ' ') {
				next
			} else {  # device not found, so can't find last record
				write (ttyout,201)
				iopcon(ipx:ipx) = '1'
				do ik = ipx+1, iipx-1 {
					iopcon(ik:ik) = ' '
				}
				go to 300
			}
		}
	}
	if (ifound == 0) {       # device not found, so can't find last record
		iopcon(ipx:ipx) = '1'
		do i = ipx+1, iipx-1 {
			iopcon(i:i) = ' '
		}
	}
300	igo = 0

#	if (ioutverbose == 0) {
#		write (ttyout,500) filreq, fiobox
#500		format (' DEBUG: filreq=',i6,',   ',i6,',   ',i6,/,
#                '        fiobox=',i6,',   ',i6,',   ',i6)
#		write (ttyout,*) 'iopcon=', iopcon
#	}

	return

1000	igo = 1000
	return

200	format(i6)
201	format (' WARNING: could not fit decoded last record number',
		' into command line.  Set to 1.')

%5	format (' INVALID PIXEL SPECIFICATION, reenter line',/,a1,$)
	end
