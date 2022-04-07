#  4.7.80  dmc  upprot  sfortran
#
#    update specpr restart file protection.
#
      subroutine upprot(idw,iwprot)
	  implicit integer*4 (i-n)

      common /lblprt/ iprtu,iprty,iprtd,iprtv,iprtw,iprts
#
#
#
	if (idw == 3) {
		iprtu = iwprot
	} else if (idw == 4) {
		iprty = iwprot
	} else if (idw == 7) {
		iprtd = iwprot
	} else if (idw == 9) {
		iprtw = iwprot
	} else if (idw == 8) {
		iprtv = iwprot
	}
	return
	end
