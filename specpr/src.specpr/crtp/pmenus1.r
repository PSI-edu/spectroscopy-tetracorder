	subroutine pmenus1 (wavshift)

	implicit none

#ccc  version date: 11/26/2018
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine write other  menu ino on the plot
#ccc            added overlays 12/22/2009
#ccc            added wavelength markers 9/2013
#ccc            3pt band depths and tetracorder features 11/2018
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc     argumrnts: wavshift
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

include "../common/spmaxes"   # max parameters, must be first

include "../common/blank"
include "../common/lbl7"
include "../common/lbl6"
include "../common/lbl4"
include "../common/label1"
include "../common/labl2"
include "../common/lbl3"
include "../common/label3"
include "../common/labelf"
include "../common/lblg"
include "../common/hptrm"
include "../common/lundefs"
include "../common/alphabet"
include "../common/dscrch"
include "../common/overlys"
include "../common/sp3pfeat"
include "../common/tetfeat"
include "../common/wavemarks"
include "../common/deletep"
include "../common/spcolors"

character*7     ovcolor
character*7     wmcolor
character*8 aname
character*80 outline        # for X-window writes
character*180 outlinb        # intermediate buffer for X-window writes

integer*4 iov        #overlay number
integer*4 iwm        #wavelength marker number
integer*4 il, i, j, itmpo, iero
real*4    wavshift   # wavelength shift (not saved, initialized to zero each time the routine is entered.
integer *4 ipt, ibd, itw
real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax


#ZZZZZZ
			    if ( ovflgmenu == 1) {    # put overlay info on plot window
                                for (iov=1; iov<=6; iov=iov+1) {
                                        if (ovrflg(iov) > 0) {

                                                if (iov == 1) ovcolor='red    '
                                                if (iov == 2) ovcolor='blue   '
                                                if (iov == 3) ovcolor='green  '
                                                if (iov == 4) ovcolor='orange '
                                                if (iov == 5) ovcolor='cyan   '
                                                if (iov == 6) ovcolor='magenta'
						if (igrmod >= 50 && igrmod <= 53) {
#XWIN 							call xset_color(iov+1)   # 2=red, 3=blue, 4=green, etc
						}
                                                write (outlinb,1234) iov,
                                                        ovfil(iov), ovrec(iov),
                                                        ovwfil(iov), ovwrec(iov),
                                                        ovops(iov), ovcolor,
                                                        ovtitle(iov), ovrchn(iov)
						# outlinb is 180 char buffer in case of overflow
						outline=outlinb(1:80)
						call gwrite(outline)
						if (igrmod >= 50 && igrmod <= 53) {
#XWIN 							call xset_color(0)   # black
						}
                                        }
                                }
			    }
				#write (*,*) "DEBUG: wmflgmenu=", wmflgmenu
			    if ( wmflgmenu == 1) {   # put wavelength marker info on plot window
                                for (iwm=1; iwm<=6; iwm=iwm+1) {
                                        if (wmrflg(iwm) > 0) {

                                                if (iwm == 1) wmcolor='red    '
                                                if (iwm == 2) wmcolor='blue   '
                                                if (iwm == 3) wmcolor='green  '
                                                if (iwm == 4) wmcolor='orange '
                                                if (iwm == 5) wmcolor='cyan   '
                                                if (iwm == 6) wmcolor='magenta'
						if (igrmod >= 50 && igrmod <= 53) {
#XWIN 							call xset_color(iwm+1)   # 2=red, 3=blue, 4=green, etc
						}
						#write (*,*) "DEBUG: wmrflg(iwm)=", wmrflg(iwm), wmcolor
                                                write (outlinb,1235) iwm,
                                                                wmops(iwm),
                                                                wmcolor,
                                                                wmrwav(1:wmnum(iwm),iwm)
						# outlinb is 180 char buffer in case of overflow
						outline=outlinb(1:80)
						call gwrite(outline)
						if (igrmod >= 50 && igrmod <= 53) {
#XWIN 							call xset_color(0)   # black
						}
                                        }
                                }
			    }

			    if ( bdflgmenu == 1) {   # 0=off, 1=on to print band depths on plot
				for (ibd=1; ibd<=imax3pt; ibd=ibd+1) {

				    if (sfmode(ibd) > 0 && sonoff(ibd) > 0) {
					write (outlinb,1237) ibd, sfeatname(ibd),
							sbdepth(ibd)
						outline=outlinb(1:80)
						call gwrite(outline)
				    }
				}
			    }
1237    		    format(1x,'bd', i1,'=',a,'  ' ,f8.4)

				#tetflgmenu = 1
			    if ( tetflgmenu == 1) {   # 0=off, 1=on to print tetracorder features on plot

				#write (ttyout,*) "debug: wriout: tetflgmenu == 1"
				for (itw=1; itw<=imaxtet; itw=itw+1) {

				    if (tetonoff(itw) > 0 && tfmode(itw) > 0) {
					#write (ttyout,*) "debug: wriout: tetfline=", tetfline(itw)
					outline= tetfline(itw)
					call gwrite(outline)
				    }
				}
			    }

############ 		    write overlay info to text window
				for (iov=1; iov<=6; iov=iov+1) {
					if (ovrflg(iov) > 0) {

						if (iov == 1) ovcolor='red    '
						if (iov == 2) ovcolor='blue   '
						if (iov == 3) ovcolor='green  '
						if (iov == 4) ovcolor='orange '
						if (iov == 5) ovcolor='cyan   '
						if (iov == 6) ovcolor='magenta'
						write (ttyout,1234) iov,
							ovfil(iov), ovrec(iov),
							ovwfil(iov), ovwrec(iov),
							ovops(iov), ovcolor,
							ovtitle(iov), ovrchn(iov)
					}
				}
1234    			format(1x,'ov', i1,'=',a1,i6,' ',a1,i6,' ',a2,' ',a,' ',a,i5)

############		    write wavelength markers to text window
				for (iwm=1; iwm<=6; iwm=iwm+1) {
					if (wmrflg(iwm) > 0) {

						if (iwm == 1) wmcolor='red    '
						if (iwm == 2) wmcolor='blue   '
						if (iwm == 3) wmcolor='green  '
						if (iwm == 4) wmcolor='orange '
						if (iwm == 5) wmcolor='cyan   '
						if (iwm == 6) wmcolor='magenta'
						write (ttyout,1235) iwm,
								wmops(iwm),
								wmcolor,
								wmrwav(1:wmnum(iwm),iwm)
					}
				}
1235    			format(1x,'wm', i1,'=',a4,' ',a7,' ',6f12.5)

############		    write tetracorder features to text window
				for (itw=1; itw<=imaxtet; itw=itw+1) {

				    if (tetonoff(itw) > 0 && tfmode(itw) > 0) {
					write (ttyout,*) tetfline(itw)
				    }
				}


############                write wavelength shift info in text window and graphics window if >0.0
				write (ttyout,1236) wavshift
				if (abs(wavshift) > 0.1e-9) {
					write (outlinb, 1236) wavshift
					outline=outlinb(1:80)
					call gwrite(outline)
				} 
1236				format (' Wavelength shift, wavshift=' f12.7)
#ZZZZZZZZZZZZZZ
	return
	end
