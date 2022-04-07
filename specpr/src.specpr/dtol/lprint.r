subroutine lprint(mode,ia,ib,ic,id,itf,ih,im,irec,lpline,itw,inm)
#	Ratfor
#********************** subroutine lprint ***********************


#ccc  name: lprint
#ccc  version date: 10/01/1985
#ccc  author(s): Mat Klejwa
#ccc  language: Ratfor
#ccc
#ccc  short description: print to line printer: header and data
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

	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"	
	include "../common/lbl6"

	integer*2 chkbit,ibit
	character*8 inm

	ibit=1
	itmp=chkbit(icflag,ibit)

	call plpage(lpline,inm)

	if (itmp==1) {
			write(lstlun,5) irec, ititl, itxtch
			lpline=lpline+1
			if (itf==1)  {
				call ptxt1(itf,j,lpline)
				call plpage(lpline,inm)
			}			
			itw=1
	}else {
		if (mode==1) { 
#			telescopic data print mode
			call pdata(ia,ib,ic,id,itf,ih,irec,lpline)
		}else {
#		      	lab print mode
	        	call pdatl1(ia,ib,ic,id,itf,ih,im,irec,lpline,itw,inm)
		}
		itw=0
	}

5	format(i6, 2x, a, 5x, i5,' Characters of TEXT')
	return
	end
