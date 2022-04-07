   subroutine gtfl(id,irec,il)

#   reads in the data set

	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lbl7"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/lblg"


40	call crtin
	i = 1
	call wjfren (i,x,id)
	call wjfren (i,x,il)
	if (id == ihx || id == ihe) {
		il = id
		go to 10000
	}
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
		call what(i)
		go to 40
	}
	if (x==0 & id==0) {
		id = itrol(1)
		 x = float(itrol(2))
	    }
	if (id == ihcv) id = ihv
	if (id == ihcw) id = ihw
	if (id == ihcd) id = ihd
	if (id == ihcu) id = ihu
	if (id == ihcy) id = ihy

	if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
50		format (' ERROR record number out of range, reenter')
		go to 40
	}
	irec = x
	call devok(4,id,irec,lun,ier)
	if (ier != 0) {
		write(ttyout,55)
55		format('REENTER')
		goto 40
	}
	itmp = irec
	call redfil(itmp,lun,ier)
	if (ier != 0) go to 40

	if (nchans < 1) {
		write (ttyout, 60) nchans
60		format (' TOO FEW channels:',i6, /, 'Redo',/)
		go to 40
	}


10000	return
	end 
