	subroutine rbline(in)
	implicit integer*4 (i-q)

	include "../common/hptrm"
#RED
	integer*4 iwrite     # function iwrite

	if (igrmod >= 99) return
	call sb(0)
	if (igrmod != 20 && igrmod != 21) {  #HP2623
		if (in==1) {
			ihpout(1:4)=char(27) // '*dM'
			iot = 4
		} else {
			ihpout(1:4)=char(27) // '*dN'
			iot=4
		}
		ii = iwrite(1,iot,ihpout)
		iot=0
	}
	return
	end
