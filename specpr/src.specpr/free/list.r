	subroutine list(inchar)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#       this routine lists the contents of the history file     #
#################################################################
	include "../common/cmd"
	include "../common/lundefs"
	integer*4 lnb

	character*80    idummy

	call wjfren(inchar,x,ilp)
	ipage = x
	call hreset(1)
	ipage = min0(5,max0(1,ipage))
	write(ttyout,'(''Page:'',i3)') ipage
	if (ipage==1) {
		ix = icoman
		do ii=1,20 {
			ix = mod(ix-1,20)
			if (ix==0) ix = 20
			read(cmdlun,rec=ix,err=1) idummy
			do i=1,lnb(idummy) {
				if (idummy(i:i)==char(0)) {
					write (idummy(i:i),'(" ")')
				}
			}
			write(ttyout,2) ii,idummy(1:lnb(idummy))
1			next
		}
	} else {
		ipage = ipage - 1
		do ii=20*ipage+1,20*ipage+20 {
			read(cmdlun,rec=ii,err=4) idummy
			write(ttyout,2) ii,idummy(1:lnb(idummy))
4			next
		}
	}
	write(ttyout,3)
	return

2       format (1x,i2,1x,a)
3       format (' ?=list; $n=exec line n; %n x=move line ',
		'n to line x;=n =type in line n',/,
' <filename startline# endline# = read input from file',/,
' >filename = copy input to file; > = end copy mode.')
	end
