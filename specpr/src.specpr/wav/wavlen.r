	subroutine wavlen
	implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this is the main wavelength assignment routine          c
#                                                               c
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"


	repeat {
		call hreset(1)
		call whedr
		write(ttyout,10)
		write(ttyout,20)
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihw) call wav100
		if (il==ihv) call wav101
		if (il==ihp) call wav150
		if (il==ihr) call wav200
		if (il==iht) call wav300
		if (il==ihy) call wav400
		if (il==ihc) call wav500
		if (il==ihd) call wav600
		if (il==ihl) {
			repeat {
				write(ttyout,30)
				call crtin
				j = 1
				call wjfren(j,f,id)
			} until(j<80)
			if (id==ihc) lun = ttyout
			if (id==ihl) lun = lstlun
			write(ttyout,40)
			call crtin
			j = 1
			call wjfren(j,f,il)
			call wjfren(j,f2,il2)
			ist = f
			ifin = f2
			if (j>=80) ifin = f
			if (ifin>99) ifin = 99
			if (ist<1) ist = 1
			nchsav = nchans
			call prwav(lun,ist,ifin)
			nchans = nchsav
		} else if (il==iha)
			repeat {
				call wavfil(iwavfl)
				call redhed(0,iwavfl,ier)
				call wavlng(iwavfl)
				itrol(2) = iwavfl
				write(ttyout,50)itrol(2),nchans
				write(ttyout,60)
				call crtin
				i = 1
				call wjfren(i,x,ic)
			} until(ic!=ihc)
		else {
			if (il==ihe || il==ihx) break 1

		}
	}
	return

10      format(1x,10('*'),1x,'wavelength assignments',1x,10('*'),/)

20      format(
'  type:  w  to assign ir  cvf wavelengths',/,
'         v  to assign vis cvf wavlengths',/,
'         p  to assign pds photometer nominal wavelengths',/,
'         r  to read a record and put the data ',
				'into the wavelength file',/,
'         t  to write a wavelength record to disk or tape',/,
'         y  to type in your own wavelenghts',/,
'         c  to change the number of channels',/,
'         d  to edit a wavelength record',/,
'         l  to print summary of all wavelength records',/,
'         a  to assign a different wavelength record',/,
'         e  or  x  to exit routine',/)

30  format(//,
' type  c  for crt listing',/,
'       l  for line-printer',/)

40  format(/,
'type the beginning and ending record number',/)

50  format(' *** assigning wavelength record',i3,5x,'number of channels =',i4)

60  format(' type  c  to change, or  return to continue',/)

	end
