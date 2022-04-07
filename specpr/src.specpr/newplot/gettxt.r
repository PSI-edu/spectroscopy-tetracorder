	logical function gettxt(ii)
	implicit integer*4 (i-n)

#ccc  name: gettxt
#ccc  version date: 8/15/83
#ccc  author(s): J.A.Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: 
#ccc		This routine reads text to be placed on the plot
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: crtin, wjfren, getstr
#ccc  argument list description: ii --- dummy argument
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#  textx(i) contains x-position of i-th text string or symbol
#  texty(i)    "     y   "      "   "     "     "   "     "
#  texta(i) is set to 1 if text angle==90 degrees. set to 0 if 0 degrees
#           or symbol type
#  texts(i) is start position of i-th string in textbuf or -1 if symbol
#  texte(i) is end position of i-th string in textbuf or symbol size

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/lbl4"
	include "../common/pltcnt"
	include "../common/alphabet"
#RED
	integer*4 lnb     # function lnb

	integer ctext
	real*4	x

	write(ttyout,10)

	do i=1,15 {
		textx(i) = 0.0
		texty(i) = 0.0
		texts(i) = 0
		texte(i) = 0
		texta(i) = 0
	}
	nxtchr = 1
	ctext = 1
	repeat {
		write(ttyout,30) ctext
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihx || il==ihe) {
			gettxt=.false.
			return
		}
		if (il==ihc) {
			gettxt=.true.
			return
		}
		if (il==ihr) {
			if (ctext>1) ctext = ctext - 1
		} else if (ctext > 15){  #max of 15 strings
			write(ttyout,40)
		} else if (il==ihs) {   #symbol entry...
								#iopcon contains tsize,x&y  pos.
			call wjfren(i,x,il)
                        if (il!=0 || x<=0 || x>15) goto 100
			texta(ctext) = x
			texts(ctext) = -1
			call wjfren(i,x,il)
			if (il!=0 || i>=80 || x<=0 || x>6) goto 100
			texte(ctext) = x
			call wjfren(i,x,il)
			if (il!=0 || i>=80 || x<=0 || x>25) goto 100
			if (penplt == 0) { # gould class plotter
				textx(ctext) = x
			} else {           # HP 7550 class plotter
				textx(ctext) = x / 1.02
			}
			call wjfren(i,x,il)
			if (il!=0 || i>=80 || x<=0 || x>19) goto 100
			if (penplt == 0) { # gould class plotter
				texty(ctext) = x
			} else {           # HP 7550 class plotter
				texty(ctext) = x / 0.79
			}
			ctext = ctext + 1
			next
		}else if (il==0 && i<80 && x<=25. && x>=0.) {
			if (penplt == 0) { # gould class plotter
				textx(ctext) = x        #text entry...
			} else {           # HP 7550 class plotter
				textx(ctext) = x / 1.02
			}
			call wjfren(i,x,il)     #iopcon contains x,y,angle,text
			if (il!=0 || i>=80 || x<0. || x>19.) goto 100
			if (penplt == 0) { # gould class plotter
				texty(ctext) = x
			} else {           # HP 7550 class plotter
				texty(ctext) = x / 0.79
			}
			call wjfren(i,x,il)
			if (il!=0 || i>=80 || (x!=0.0 && x!=90.0)) goto 100
			texta(ctext) = int(x) / 90
			texts(ctext) = nxtchr
			lstchr = lnb(iopcon)
			nchars = lstchr-i+1
			text(nxtchr:nxtchr+nchars) = iopcon(i:lstchr)
			nxtchr =  nxtchr + nchars
			texte(ctext) = nxtchr-1
			ctext = ctext+1
			next
		}
100		write(ttyout,20)
	}

10      format(' TEXT: input x and y coordinates in cm,  ',
			'angle in degrees (90.=normal),',/,
'       then text. 15 strings may be entered. ',
				'Type  c  to CONTINUE.',/,
' symbol entry: type  s  then type, size and x and y position. (in cm.)'/)

20      format(' ** REENTER TEXT **')

30      format(' Text Entry ',i2,':'/)

40	format(' ** SORRY, only a MAX OF 15 STRINGS may be entered **')
	end
