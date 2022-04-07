	subroutine tabpos(ichk,iix,iiy)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:    none
#ccc  argument list description:
#ccc     arguments: ifd,iix,iiy
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/lbl4"

#	data ihplus/'+'/
	ihplus = ihchar('+')

	ichk=0
1	write (6,*) '*s33^'
	read (5,100,iostat=ier) iopcon
	if (ier!=0) {
		write (6,*) 'read error'
		return
	}
100	format(a)
	i=1
	call wjfren(i,a,il)
	if (il==ihplus) {
		call wjfren(i,a,il)
		iix=a
		i=i+2
		call wjfren(i,a,il)
		iiy=a
		call movabs(0,300)
		call sb(0)
		call texmod
		write (ttyout,*) ''
		return
	}
	if (il==ihx || il==ihy || il==ihd) {
		ichk=il
		return
	}
	go to 1
	return
	end
