	subroutine chginf (inxt)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine lists information on
#ccc                   the crt to a data record and allows the usr to
#ccc                   change the information ( but not the data )
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren,what,hreset,eralph
#ccc  subroutines called from: crtp/wriout, hed/chinfo.r
#ccc  argument list description:
#ccc      argument: inxt
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#  this routine lists the infromation on the crt to a data record      #
#  and allows the user to change the information (but not the data).   #
#                                                                      #
########################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/alphabet"

	character*74	mmhist(4)
	equivalence (mmhist,mhist)

	integer*2 chkbit, ibit

	data ihbl/1h /

	if (iprom==0) iblank = 0
	else iblank = ihbl
	repeat {
		inxt= 0
		ibit = 1
		if (chkbit(icflag,ibit) == 1) {       # text record
			call eralph
			write (ttyout,1) idv1, ifl1, ititl
			if (itxtch < 1) itxtch = 1
			write (ttyout,300) ititl, usernm, itxtpt, itxtch
			call crtin
			i= 1
			call wjfren (i,x,il)
			ix= x+0.5
			if (il==ihx || il==ihe || il==ihg) {
				inxt= il
				return
			}
			else if (x==0 && il==0) {
				inxt = 1
				return
			}
			else if (il==ihu) {        # username change
				write(ttyout, 301)
				call crtin
				usernm = iopcon(1:8)
			}
			else if (il==iht) {        # title change
#timj
#   				write(ttyout, 101) iblank
				write(ttyout, 101) 
				call crtin
				ititl = iopcon
			}
			else if (il==ihp) {        # text pointer
				write(ttyout, 302)
				call crtin
				i= 1
				call wjfren (i,x,il)
				if (il!=0) {
					call what(i)
					call crtin
					next
				}
				if (x < 0 || x > maxrec) {
					write (ttyout,303)
					call crtin
					next
				}
				itxtpt = x
			}
			else if (il==ihc) {        # number of chars
				write(ttyout, 402)
				call crtin
				i= 1
				call wjfren (i,x,il)
				if (il!=0) {
					call what(i)
					call crtin
					next
				}
				if (x <= 1 || x > maxtxt) {
					write (ttyout,303)
					call crtin
					next
				}
				itxtch = x
			}
		} else { 				# data record
			call eralph
			write(ttyout,1) idv1, ifl1, ititl
			airmas = irmas/ 1000.0
			write(ttyout, 2) ititl, ihist, mmhist, airmas, usernm
			call crtin
			i= 1
			call wjfren (i,x,il)
			ix= x+0.5
			if (il==ihx || il==ihe || il==ihg) {
				inxt= il
				return
			}
			else if (x==0 && il==0) {
				inxt = 2
				return
			}
			else if (il==iha) {
				write(ttyout, 276)
				call crtin
				i= 1
				call wjfren (i,x,il)
				if (il!=0) next
				airmas= x
				x= x*1000.0 +0.5
				if (x>32000.) x= 32000.
				if (x<-32000.) x=-32000.
				irmas=x
			}
			else if (il==iht) {
				write(ttyout, 101) 
				call crtin
				ititl = iopcon
			}
			else if (il==ihh) {
#timj  			     write(ttyout, 151) iblank
       			     write(ttyout, 151)
				call crtin
				ihist = iopcon
			}
			else if (il==ihu) {        # username change
				write(ttyout, 301)
				call crtin
				usernm = iopcon(1:8)
			}
			else if (il==ihm) {
				call wjfren (i,x,il)
				ix= x+0.5
				if (ix<=4 && ix>=1) go to 250
				write(ttyout, 201)
				do n= 1,4 {
					j = (n-1)*74+1
					j2 = j+73
					call crtin
					mhist(j:j2) = iopcon
				}
			}
			else if (x>=1. && x<=4.) {
250    			     write(ttyout, 251) ix
				j = (ix-1)*74+1
				j2=j+73
				call crtin
				mhist(j:j2) = iopcon
			} else {
				call what(i)
			}
		}
	}

1       format (15x,'Header Information Display and Change Routine',/,
		15x,45(1h-), /,
		15x, 'display of: ', a1, i6,3x,  a, /,
		' type in the number and/or letter code next ',
			'to the information you', /,
		' wish to change', /)
2       format ('  t: title=', 3x, a, /,
		'  h: history= ' ,a, //,
		'  m: manual history: ',
		      '(type m and the line number to change only one line)', /,
		2x, 73(1h-), /,
		4(1x, a, /),
		2x, 73(1h-), /,
		'  a: airmass=  ', f8.4, /,
		'  u: username= ',a, //,
		' type  g  to exit to crt plot,',
			'  e  to soft exit with write and no plot,'/,
		'  x  to hard exit,',
		' press return to display next page of information', /)
101     format (' type in title, 40 characters',/,1x,39(1h-),1hI,/ )
#151     format(' type in history, 60 characters',/,1x,a,59(1h-),1hI,/)
151     format(' type in history, 60 characters',/,1x,59(1h-),1hI,/)
201     format (' type in the manual history - 4 lines at 73 ',
		'characters per line', /)
251     format (' type in one line (73 characters) to replace line',
		i2, ' of the', /, ' manual history', /)
276     format (' type in the airmass', /)
300     format ('  t: title=', 3x, a, /,
		'  u: username= ' ,a, //,
		'  p: additional text pointer: ', i6,/,
		'  c: number of characters of text: ', i6, //,
		' type  g  to exit to crt plot,',
			'  e  to soft exit with write and no plot,'/,
		'  x  to hard exit,',/,
		' press return to display other information', /)
301	format (' type in the username, 8 characters',/,1x, 7('-'),'I',/)
302	format (' type in the new text pointer record number',/)
303	format (' pointer out of range.  type return to continue',/)
402	format (' type in the number of characters of text',/)
	end
