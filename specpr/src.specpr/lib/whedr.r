	subroutine whedr
	implicit integer*4 (i-n)
#############################################################

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    texmod,blanks,end
#ccc  argument list description:
#ccc        arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
####################################################################
 
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/lundefs"
	include "../common/alphabet"

	integer*4 crindex

	call texmod
	write(ttyout,10) isavt,isavf,iwdgt,iwjf,iwrkt,iwrkf
	write(ttyout,15) inmu,isvcu,inmy,iwjcy,istrf
	balat=alat*57.29578

	i = crindex(ilfl,'/')
	if (i==0) i=1

	write(ttyout,19) ilfl(i:i+14),balat,nchans,itrol(1),itrol(2),itrol(3)
	write(ttyout,22) iprtv,iprtw,iprtd,iprtu,iprty,iprts,iline
	return


10      format (' v = ',a8,': f',i6,4x,'w = ',a8,': f',i6,
		4x,'d = ',a8,': f',i6)

15      format (' u = ',a8,': f',i6,4x,'y = ',a8,': f',i6,4x,
		's = starpack: f',i6)

19      format (' lp: ',a14,
		'  obs lat=', f8.3, ' deg  channels=', i5,
		' wav fl=', a1, i6, 1x, a)

22      format (' file protect: v',i7,',w',i7,',d',i7,',u',i7,
		',y',i7,',s',i4,' ltype=',i2,/,
		74(1h-))

	end
