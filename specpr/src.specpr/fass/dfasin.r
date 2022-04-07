	subroutine dfasin
	implicit integer*4 (i-n)

#ccc  version date: 04/09/85
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine sets up the file/device assignments.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc         eralph,crtin,wjfren,whedr,getstr,setfil,iclose,
#ccc         rewind,newsta
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
#####################################################################
#                                                                   #
#   this routine sets up the file/device assignments.               #
#                                                                   #
#####################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/labelf"
	include "../common/lblvol"
	include "../common/alphabet"
	include "../common/filenames"
	include "../common/lundefs"
	include "../common/ioftyp"

	character*80    anull,dummy(7)
	equivalence     (ivfl,dummy(1))

	integer*4 ier, idummy
	integer*2 irer, islun
	logical fexist, fop

	data anull                /NULL/


	repeat {
		call eralph
		call whedr
		write (ttyout,1)
		write (ttyout,20) ivfl(1:74)
		write (ttyout,25) iwfl(1:74)
		write (ttyout,30) idfl(1:74)
		write (ttyout,35) isfl(1:74)
		write (ttyout,40) iufl(1:74)
		write (ttyout,45) iyfl(1:74)
		write (ttyout,50) ilfl(1:74)
		write (ttyout,61)
		call crtin
		i=1
		repeat {
			call wjfren (i,x,il)
			if (i>=80) break
			ier = 1
			iergst = 1

			if (il == ihv) {
				ilun = 8
				ifile = 1

			} else if (il == ihw) {
				ilun = 9
				ifile = 2

			} else if (il == ihd) {
				ilun = 7
				ifile = 3

			} else if (il == ihs) {
				ilun = 17
				ifile = 4

			} else if (il == ihu) {
				ilun = 3
				ifile = 5

			} else if (il == ihy) {
				ilun = 4
				ifile = 6

			} else if (il == ihl) {
				ilun = 12
				ifile = 7

			} else if (il == ihe | il == ihx) {
				call chkdev
				return

			} else {
				write(ttyout,"('INVALID INPUT, reenter')")
				break
			}

			while (iergst==1) call getstr(i,
				dummy(ifile),iergst)
			close(ilun,iostat=idummy)

#
#	check for t=3d: not normal specpr file: it is 3d
#
#DEBUG			write (ttyout,*) 'ilun=',ilun, 'ftptr=', ftptr
#DEBUG			write (ttyout,*) 'ftptr(ilun)=', ftptr(ilun)
#DEBUG			write (ttyout,*) 'filtyp=', filtyp
#
# set filtyp for valid files:
#
			if (ilun == ulun | ilun == ylun | ilun == dlun |
				ilun == vlun | ilun == wlun) {
				filtyp(1,ftptr(ilun)) = 0   # normal specpr file
			}
			call wjfren (i,x,il)
			if (il == iht) {
				if (iopcon(i:i+2) == '=3d') {
					filtyp(1,ftptr(ilun)) = 3  #3d type file
				}
			} else {
				i = i - 1
				if (i < 1) i =1
			}
					

			ier = 0
			if (ilun ==  ulun | ilun == ylun | ilun == dlun | 
					ilun == vlun | ilun == wlun) {
				if (dummy(ifile) != anull) {
					if (filtyp(1,ftptr(ilun)) == 0) {
						inquire(file=dummy(ifile),
							iostat=ier,
							exist=fexist,
							opened=fop)
						if ( ! fexist) {
							write (ttyout,75)
							call crtin
							i=1
							call wjfren(i,x,il)
							if (il == ihe) {
								dummy(ifile)=anull
								break
							}
							if (il == ihx) {
								dummy(ifile)=anull
								go to 900
							}
							if (il != ihy) {
								dummy(ifile)=anull
								break
							}
						} else {
							write (ttyout,76)
						}
						open(ilun,
							file=dummy(ifile),
							access='direct',
							recl=1536,
							form='unformatted',
							iostat=ier)
					} else if (filtyp(1,ftptr(ilun)) == 3) {
						call file3d (ilun,dummy(ifile),
								ier,ier2)
						if (ier2 == ihe | ier2 == ihx) {
							dummy(ifile)=anull
							break
						}
					}
				}

			} else if (ilun ==  lstlun) {
				if (dummy(ifile)!=anull) {
					inquire(file=dummy(ifile),
						iostat=ier,
						exist=fexist,
						opened=fop)
					if ( ! fexist) {
						write (ttyout,75)
						call crtin
						i=1
						call wjfren(i,x,il)
						if (il == ihe) {
							dummy(ifile)=anull
							break
						}
						if (il == ihx) {
							dummy(ifile)=anull
							go to 900
						}
						if (il != ihy) {
							dummy(ifile)=anull
							break
						}
					} else {
						write (ttyout,76)
					}
					open(ilun,
						file=dummy(ifile),
						form='formatted',
						iostat=ier)
				}
			} else if (ilun ==  slun) {
				if (dummy(ifile)!=anull) {
					inquire(file=dummy(ifile),
						iostat=ier,
						exist=fexist,
						opened=fop)
					if ( ! fexist) {
						write (ttyout,75)
						call crtin
						i=1
						call wjfren(i,x,il)
						if (il == ihe) {
							dummy(ifile)=anull
							break
						}
						if (il == ihx) {
							dummy(ifile)=anull
							go to 900
						}
						if (il != ihy) {
							dummy(ifile)=anull
							break
						}
					} else {
						write (ttyout,76)
					}
					open(ilun,
						file=dummy(ifile),
						access='direct',
						recl=77824,
						form='unformatted',
						iostat=ier)
				}
			}
			if (ier==0 && dummy(ifile)!=anull) {
				islun = ilun
				irer = 0
				if (dummy(ifile)(1:7) == '/dev/mt') {
					call rewinf(islun,irer)
				}
				call newsta(ilun,14,1)
				write (ttyout, 77)
				call crtin
			} 
			if (ier!=0 && irer!=25) {       #irer=25 means not tape
				write(ttyout,60) dummy(ifile), ier, ilun
				call crtin
				dummy(ifile) = anull
				break
			}
		}
	}
900     continue
1	format (15x, '*** FILE ASSIGNMENTS ***', /, 15x,
	35(1h-),/, 1x, 'to reassign files type letter and name:',
		/, 4x,    '(74 characters max per file name)')
20	format (1x, 'v = ', a)
25 	format (1x, 'w = ', a)
30 	format (1x, 'd = ', a)
35	format (1x, 's = ', a)
40 	format (1x, 'u = ', a)
45 	format (1x, 'y = ', a)
50 	format (1x, 'l = ', a)
60 	format (' Can''t open ',a,/, 
		' Error ', i4, '  logical unit:',i4, /,
		' Hit return to continue',/)
61 	format (5x, 'e  or  x = EXIT this routine', /)
75	format (1x, 'WARNING: File does not exist.  ',
			'Do you wish to create it?',/,
			1x, 'Type  y  for YES CREATE, ',
					'any other key exits.')
76	format (1x, 'File already exists...  attempting to open.')
77	format (1x, 'File opened.   Press return to continue.')
	end
