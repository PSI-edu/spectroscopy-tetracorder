   subroutine getk2(jlyr, iminr,il,mmixflag)

#	get absorption coefficient from specpr file

	implicit integer*4 (i-n)
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "defs.h"
	include "lmrefl.h"

	integer*4 mmixflag, jloop, mlayers, jlyr

        mlayers = 1           # temp variable for layers in actual use
        if ( nlayers > 1 ) {
                mlayers = nlayers
        }


50	format (' ERROR record number out of range, reenter',/)

10	if (mmixflag == 0) {

100	 if (nlayers > 1) {
	     write (ttyout, 104) jlyr, iminr
104	     format (/,'type in the file ids and record numbers of the ',
		' ABSORPTION COEFFICIENT,',/,
		' for layer ',i2,' element ',i2)

	 } else {
	     write (ttyout, 105) iminr
105	     format (/,'type in the file ids and record numbers of the ',
		' ABSORPTION COEFFICIENT,',/,
		' for element ',i2)
	 }

	  call crtin
	  i=1
	  call wjfren (i,x,id)
	  call wjfren (i,x,il)
	  if (id == ihe || id == ihx) {
		il = id
		go to 10000
	  }
	  if (il == ihe || il == ihx) go to 10000
	  if (il != 0) {
		call what(i)
		go to 100
	  }
	  if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
		go to 100
	  }
	  irecxk(iminr)         = x    #record number for xk set iminr
	  irecxklyr(jlyr,iminr) = x    #record number for xk set iminr
	  idxk(iminr)           = id
	  idxklyr(jlyr,iminr)   = id
	  call devok(4,id,irecxk(iminr),lun, ier)
	  if (ier != 0) {
		write (ttyout,120)
120		format (' REENTER',/)
		go to 100
	  }
	  itmp = irecxk(iminr)
	  call redfil (itmp,lun,ier)
	  if (ier != 0) {
		write (ttyout,120)
		go to 100
	  }
	  write (ttyout,125) iminr, ititl, itchan
125		format (/,'ABSORPTION COEFFICIENT set',i3,/, 5x,
  			a, 5x, 'channels=',i6,/)

	  do j = 1, nchans {
		xk(iminr,j)         = data(j)
		xklyr(jlyr,iminr,j) = data(j)
	  }
	  xktitl(iminr) = ititl

	} else if ( mmixflag == 1 || mmixflag == 2 || mmixflag == 3 ) {

	  do jloop = 1, nmolmix(iminr) {

500	    if (nlayers > 1) {
		write (ttyout, 504) jlyr, iminr, jloop
504		format (/,'type in the file ids and record numbers of the ',
			' ABSORPTION COEFFICIENT,',/,
			' for layer ',i2,' element ',i2,/,
			' for component ',i3,' to be mixed in the grain',/,
			' Followed by the abundance of the component')

	    } else {
		write (ttyout, 505) iminr, jloop
505		format (/,'type in the file ids and record numbers of the ',
			' ABSORPTION COEFFICIENT,',/,
			' for element ',i2,/,
			' for component ',i3,' to be mixed in the grain',/,
			' Followed by the abundance of the component')
	    }

		if (jloop == 1 ) {
		  write (ttyout, 506)
506               format (' This it the main component of the embedded mixture')

		  write (ttyout,76)
76		  format (' For this FIRST component, you can ',
			'let it float by typing  f  for the abundance',/,
			' and let the program compute it')
		}

                if ( mmixflag == 3 && jloop == 2 || mmixflag == 2) {

                  write (ttyout, 507)
507               format (' This is the embedded particles for the EFFECTIVE MEDIUM',/,
                          ' (e.g. trace nano-phase iron index of refraction)')
                } else if ( mmixflag == 3 && jloop > 2 || mmixflag == 1 && mmixflag == 2 ) {

                  write (ttyout, 508)
508               format (' This is the MOLECULAR MIX, e.g. isotope or trace compound')
                }

		call crtin
		i=1
		call wjfren (i,x,id)
		call wjfren (i,x,il)
		if (id == ihe || id == ihx) {
			il = id
			go to 10000
		}
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 500
		}
		if (x < 1 || x > maxrec) {
			write (ttyout, 50) 
			go to 500
		}
		#if (jloop == 1 ) {                  # keep info on first entry
			irecxk(iminr)         = x    #record number for xk set iminr
			irecxklyr(jlyr,iminr) = x    #record number for xk set iminr
			idxk(iminr)           = id
			idxklyr(jlyr,iminr)   = id
		#}
		call devok(4,id,irecxk(iminr),lun, ier)
		if (ier != 0) {
			write (ttyout,120)
			go to 500
		}
		itmp = irecxk(iminr)
		call redfil (itmp,lun,ier)
		if (ier != 0) {
			write (ttyout,120)
			go to 500
		}
		write (ttyout,126) iminr, jloop, irecxk(iminr), ititl, itchan
		do j = 1, nchans {
			xmabsc(jloop,iminr,j) = data(j)
		}
		if (jloop == 1 ) {    # keep info on first entry
			xktitl(iminr) = ititl
		}
	
126		format (/,'ABSORPTION COEFFICIENT set',i3,
			' mix component',i3,/, ' record=',i7,3x,
  			a, 5x, 'channels=',i6,/)

		######### now enter the fraction to the mix

                call wjfren (i,x,il)
                if (il == ihe || il == ihx) go to 10000
		if (jloop == 1 ) {    # first entry
			if (il == ihf) {
				xmolfra(jloop,iminr)         =-1.0
				xmolfralyr(jlyr,jloop,iminr) =-1.0
			} else {

                		if (il != 0) {
                        		call what(i)
                        		go to 500
                		}
                		if (x < 0.0 || x >= 1.0) {
                       			write(ttyout,67) x
                       			go to 500
                		}
                		xmolfra(jloop,iminr)         = x
				xmolfralyr(jlyr,jloop,iminr) = x
			}
		} else {
                	if (il != 0) {
                        	call what(i)
                        	go to 500
                	}
                	if (x < 0.0 || x >= 1.0) {
                       		write(ttyout,67) x
67                     		format(1x,f12.6,' OUT OF RANGE, re-renter',//)
                       		go to 500
                	}
                	xmolfra(jloop,iminr)         = x
                	xmolfralyr(jlyr,jloop,iminr) = x
		}

	  }

	} else {

                write (ttyout,200)
200             format (/,' ERROR:',
                  ' Input not recognized (subroutine getk2).  exiting',
			' (mmixflag != 1, 2 or 3)', //)
                il=ihe
		go to 10000

	}

10000	return
	end 
