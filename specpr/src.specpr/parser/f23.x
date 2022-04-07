	subroutine f23(ic)
	implicit integer*4 (i-n)

	#ccc

	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/label1"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lbl7"

	integer parse

	call eralph
	call whedr
	write (ttyout,100)  

	
	repeat {
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (i<80) {
			if (il == ihx) {
				ic = il
				return
			
			} else if (il == ihcv | il == ihcw | il == ihcd | 
						il == ihcu | il == ihcy | il == ihcc) {
				iwtmpf=il
				call wjfren(i,x,il)
				if (i>=80 || il!=0) {
					write (ttyout,200)
					i = 0
					next
				}
				iwrec = x
				call wavlng(iwtmpf,iwrec,ier)
				if (ier!=0) {
					write (ttyout,200)
					i=0
					next
				}

			} else if (il == ihe) {
				ictrl = ihe

			} else {
				write(ttyout,200)
			}
		}
	} until (i>=80)
		
	write (ttyout,300)
	call crtin
	
	i = parse(iopcon,nchans,ictrl)

	if (i!=0) {
 		ic = ihe
		print *,' return to continue'
		call crtin
	}

	else ic = 0

	return

100	format('Math Parsing Routine.',/,
		'enter  e  to include errors,',/,
		'       wavelength ID (V,W,D,U,Y) followed by record number,',/,
		'	   or C followed by the number of channels to change',/,
		'	   the wavelength file,',/,
		'	x to exit,',/,
		'	return to continue')

200     format('Error... reenter')		

300	format('Enter equation to be calculated')

	end
