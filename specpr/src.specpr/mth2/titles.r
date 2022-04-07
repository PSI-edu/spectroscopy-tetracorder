	subroutine titles(ititl1,iopcon,ititl,itl,ier,iprodp,ibncon,bbnd,ubnd)
	implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc                   this subroutine checks for band normalization
#ccc                   or production processing turn on or off, writes
#ccc                   ititle to title file record x1, writes selected
#ccc                   title to t1
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    dread,erored,crtin,wjfren,drite
#ccc  argument list description:
#ccc  arguments: ititl1,iopcon,itit1,it1,ier,iprodp,ibncon,bbnd,ubnd
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc
	character*(*)	ititl1,iopcon,ititl,itl
	character*128	itin

	include "../common/alphabet"
	include "../common/lundefs"

	integer*4 ier

#
	ier = 0
	repeat {
		read(ttllun,rec=2,iostat=ier)itin
		call ertyp('titles',15,ier)
		itl = itin(1:40)
		write(ttyout,90)ititl1,itl
		repeat {
			call crtin
#
#     check for band normalization or production processing turn
#     on or off.
#
#timj i = 40 -> i = 41
			i = 41
			repeat {
				call wjfren(i,x,il)
				if (i>=80) break 1
				if (il==ihb&&iopcon(i:i)!='n') ibncon = 1
				if (il==ihb&&iopcon(i:i)=='n') ibncon = 0
				if (il==ihp&&iopcon(i:i)=='n') iprodp = 0
				if (il==ihp&&iopcon(i:i)=='o') iprodp = 1
				if (il==ihc) {
					call wjfren(i,x,il)
					if (il!=0) i = i-1
					else if (i<80) {
						xb = x
						call wjfren(i,x,il)
						if (il!=0) i = i-1
						xu = x
						if (xu-xb>=0.1e-15) {
							bbnd = xb
							ubnd = xu
						}
					}
				}
			}
			i = 1
			call wjfren(i,x,il)
			if (i<80) {
				call wjfren(i,x1,il1)
				if (il==ihx&&i>=80) go to 80
				if (il==ihe&&i>=80) go to 70
				if (il==iht&&il1==ihl&&x==0&&x1==0) go to 40
				if (i<=40||il!=ihl&&il!=ihp&&il!=iht||x!=0)
					break 1
				if (il==iht) {
					write(ttyout,110)
					next 1
				} else {
					if (il==ihp) go to 20
					if (il==ihl) go to 10
					if (il!=0) go to 60
				}
			}
			write(ttyout,120)
		}
		if (i<=40&&il==iht&&x==0&&il1==0&&x1>=1&&x1<=25) break 1
		ititl = iopcon(1:40)
		go to 30

10		ititl = itl
		go to 30

20		ititl= ititl1
30		i = 41
		call wjfren(i,x,il)
		if (i>=80) go to 60
		call wjfren(i,x1,il1)
		if (il==iht&&x==0&&(x1<1||x1>25)&&il1==0) {
			write(ttyout,100)
			call crtin
			i = 1
			call wjfren(i,x,il)
			if (i>=80) go to 60
			call wjfren(i,x1,il1)
		}
		if (il==iht&&x==0&&x1>=1&&x1<=25&&il1==0) go to 50
		if (il!=iht||il1!=ihl||x!=0||x1!=0) go to 60
40		call wjfren(i,x,il)
		if (x!=0||il!=0||i<40) go to 60
		call eralph
		do j = 1,25 {
			read(ttllun,rec=j+1,iostat=ier)itin
			call ertyp('titles',15,ier)
			if (ier!=0) break 1
			if (itin(128:128)==char(1))
				write(ttyout,130)j,itin(1:40)
		}
	}
	read(ttllun,rec=int(x1)+1,iostat=ier)itin
	call ertyp('titles',15,ier)
	ititl = itin
	go to 60
#
#     write ititl to title file record x1
#
50	read(ititl,150) itin(1:40)
	itin(128:128) = char(1)
	write(ttllun,rec=int(x1)+1,iostat=ier)itin
	call ertyp('titles',15,ier)
#
#     write selected title to t1
#
60	itin = ititl
	itin(128:128) = char(1)
	write(ttllun,rec=2,iostat=ier)itin
	call ertyp('titles',15,ier)
	return
70	ier = ihe
	return
80	ier = ihx
	return

90	format (' specify a title:  type  t  and the ',
		'title number (2 to 25) or:',/,
	' type  p  to preserve present title:',3x,a,/,
	' type  l  to preserve last title (t1):',1x,a,/,
	1x,39('-'),'i',5x,'title store request goes here',/)

100	format ( ' entry into title file must be record 1 to 25.',/,
' type  t  and the record number or press return to continue',/)

110	format (' recall title request must be between 1 ',
		'and 25.  enter number after t. try again.',/)

120	format (' NO TITLE entered. try again',/)

130	format (' t',i2,3x,a)

150	format (a40)
	end
