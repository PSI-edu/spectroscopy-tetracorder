	subroutine wav600
	implicit integer*4 (i-n)
#################################################################
#                                                               #
#       wavelength edit routine.                                #
#                                                               #
#       this routine allows the user to print & edit the        #
#       wavelength records.                                     #
#                                                               #
#       converted to ratfor 11/4/82 jah                         #
#                                                               #
#################################################################
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"


600 call wavfil (iwavfl)
	call wavlng (iwavfl)
	call redhed (0,iwavfl,ier)
	itrol(2) = iwavfl
	repeat {
		call hreset(1)
		write(ttyout, 603)
		call crtin
		i=1
		call wjfren (i,x,il)


		if (il ==  ihx) {
			return

		} else if (il ==  ihp) {
			call setspo
			write(lstlun, 611) iwavfl,
				nchans,datea(1),datea(2),datea(3)
			write(lstlun,612)ititl,ihist
			write(lstlun,613) mhist
			write(lstlun,616)
			ida=nchans/6.+0.9
			la=-5
			lb=0
			do i= 1, ida {
				la= la+6
				lb= lb+6
				if (lb>nchans) lb = nchans
				if (lb>256) lb= 256
				write(lstlun,614) (ic, dataa(ic), ic= la,lb)
			}
			write(lstlun,616)
			call dumpsp

		} else if (il ==  ihd) {
			repeat {
				write(ttyout,631)
				j=0
635				if(j <= 25) {
					j= j+1
640                                     call crtin
					i=1
					call wjfren (i,x,il)
					if (il==ihx) return
					if (il==ihc) go to 645
					if (il!=0) {
641                     write(ttyout, 642)
						j= j+1
						go to 640
					}
					if (x>256 |
					    x<1) {
						write(ttyout, 644)
						j= j+1
						go to 640
					}
					ich=x
					call wjfren (i,x,il)
					if (il==ihx) return
					if (il==ihc) go to 645
					if (il!=0)   go to 641
					dataa(ich)= x
					write(ttyout, 647)
					go to 635         #start loop over
				}
			}
645			call wavfil (iwavfl)
			call chginw(iwavfl)
			call wrihed(iwavfl)
			call wriwav (iwavfl)
			itrol(2)= iwavfl
			return
		}
	}
603     format (' type  p  to print wavelengths on lineprinter',/,
		'       d  to edit channels', /,
		'       x  to exit this routine', /)
611     format ('1  wavelength file', i3, 3x, '# channels=', i4,
		5x,'Date Written: ',a2,'/',a2,'/',a2)
612     format('     Title=',20a2,/,5x,'History=',30a2)
613     format('     Manual History=',/,3(5x,37a2,/),5x,37a2)
614     format (1x, 6(i3, 1x, 1pe12.5, 4x))
616     format (1x, 130(1h*))
631     format (' type in a channel number and the wavelength',/,
		' type  c  when finished', /)
642     format (' *** illegal input - retype', /)
644     format (' channel number out of bounds - retype', /)
647     format (1h+, 25x, 'continue')
	end
