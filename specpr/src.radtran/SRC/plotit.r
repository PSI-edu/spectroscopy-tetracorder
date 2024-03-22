	subroutine plotit(ipoint)
	implicit integer*4(i-n)
#ccc  CRT plotting routine modified from wriout
#ccc  subroutines called:
#ccc            scaling,crtpsc,movabs,sb,wavlng,alplty,chplta
#ccc            iwplta,crtplt,sb,movabs,unglic,what,crtin,wjfren
#ccc            dltpts,redhed,lprpct,setspool,chginf,er,pdata,
#ccc            dumpspool

	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/blank"
	include "../../src.specpr/common/lbl7"
	include "../../src.specpr/common/lbl6"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/labl2"
	include "../../src.specpr/common/lbl3"
	include "../../src.specpr/common/label3"
	include "../../src.specpr/common/labelf"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/hptrm"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/dscrch"
	include "../../src.specpr/common/overlys"
	include "../../src.specpr/common/sp3pfeat"
	include "../../src.specpr/common/tetfeat"
	include "../../src.specpr/common/wavemarks"
	include "../../src.specpr/common/deletep"


	real*4 idlt(4864)
	equivalence (idlt(1),datsc3(1))
	real lbnd
	character*8 aname
	character*80 outline        # for X-window writes
	integer*4 iscale   # = a autoscale, A= autoscale +2% range, B= autoscale, keep 0

        integer*4 iov        #overlay number
        integer*4 ochan, iiov, igo, j, i, il
	real*4 wavshift

	wavshift = 0.0  # initialize wavelength shift

9999 	ji = 80
i = 0
ibncn2 = 0
ixit = 0
iscale=ihc
#
#  find line type
#
if (idad!=2)
    do i = 1,nchans
        error(i) = 0.0
if (ictrl!=ihi) {
    if (ictrl!=iho)
        call er
    if (iauto!=ihb) {
        if (iauto!=iha) {
            igo = 8
            go to 10
        }
    }
#
#     call scaling (horizontal and vertical) routine.
#
    igo = 9
    go to 10
}
repeat {
    inxt = 0
    call chinfo(inxt)
    if (inxt==ihx)
        go to 30
    if (inxt==ihe)
        return
    if (inxt==ihg)
	go to 9999
    if (inxt!=ihr) {
        repeat {
#
#
#     calculate parameters for the plot
#
            lbnd = bbnd
            diff = ubnd-lbnd
            if (diff>0.1e-15)
                go to 20
            repeat {
                igo = 6
10              call crtpsc(igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i,iscale)
                if (igo!=4)
                    break 1
20              ig = 1
                repeat {
                    if (ji>=80) {
                        if (ictrl!=iho)
                            call er
                        if (ig==0)
                            ig = 1
                        repeat {
				call wavlng (itrol(1), itrol(2), ier)
                            if (igrmod >= 99)
                                call lpcrtp(1,nchans,lbnd,ubnd,dataa)
                            if (igrmod < 99) {
                                call movabs(0,295*2)
#
#     write  identification to the plot
#
				call movabs(0,295*2)
                                call sb(0)
				call namdev (idv1,aname)
				write(outline,40)ititl
                                call gwrite(outline)
#
#
#     draw the plot
#
                                if (igrmod >= 50 && igrmod <= 53) {
                                        call xset_color(0)   # black
                                }
                                call alplty(lbnd,ubnd)
                                diff = ubnd-lbnd
                                if (itrol(3)==iha)
                                    call wvplta(nchans,wvmax,wvmin,iline)
                                if (itrol(3)==ihn)
                                    call iwplta(nchans,wvmax,wvmin,iline)
                                xmax = wvmax
                                xmin = wvmin
                                if (itrol(3)!=iha&&itrol(3)!=ihn)
                                    call chplta(nchans,xmax,xmin,dataa,iline)

				# overlay block
			      for (iov=1; iov<=6; iov=iov+1) {
				if (ovrflg(iov) == 1) {  # straight plot, no autoscaling
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdat(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(0)   # black
				  }
				}
				if (ovrflg(iov) == 2) {  # autoscaling to 2% of range TBD
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
				  ovmin= 9.99e+33   # overlay min
				  ovmax=-9.99e+33   # overlay max
				  ochan=ovrchn(iov)
				  for (iiov=1; iiov<=ochan; iiov=iiov+1) { # finf min and max in plot range
					if (ovrwav(iiov,iov) >= xmin && ovrwav(iiov,iov) <= xmax) {
						if (ovrdat(iiov,iov) < ovmin && ovrdat(iiov,iov) > delptup) ovmin= ovrdat(iiov,iov)
						if (ovrdat(iiov,iov) > ovmax && ovrdat(iiov,iov) > delptup) ovmax= ovrdat(iiov,iov)
					}
				  }
				  # find scale and offset
					plotmax=lbnd + diff - 0.02*diff
					plotmin=lbnd + 0.02*diff
					ovoff=(plotmin - ovmin*plotmax/ovmax)/(1.0 - ovmin/ovmax)
					ovscale=(plotmax -ovoff)/ovmax
					#write (*,*) "DEBUG: ovmax=",ovmax," ovmin=",ovmin," ovscale=",ovscale," ovoff=",ovoff

				  for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # now scale the data
					if (ovrdat(iiov,iov) > delptup) {   # scale if not deleted point
						ovrdsc(iiov,iov)= ovrdat(iiov,iov) * ovscale + ovoff
					} else {
						ovrdsc(iiov,iov) = delpt   # deleted point
					}
				  }
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(0)   # black
				  }
				}
				if (ovrflg(iov) == 3) {  # autoscaling tp plot min and max
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
				  ovmin= 9.99e+33   # overlay min
				  ovmax=-9.99e+33   # overlay max
				  ochan=ovrchn(iov)
				  for (iiov=1; iiov<=ochan; iiov=iiov+1) { # finf min and max in plot range
					if (ovrwav(iiov,iov) >= xmin && ovrwav(iiov,iov) <= xmax) {
						if (ovrdat(iiov,iov) < ovmin && ovrdat(iiov,iov) > delptup) ovmin= ovrdat(iiov,iov)
						if (ovrdat(iiov,iov) > ovmax && ovrdat(iiov,iov) > delptup) ovmax= ovrdat(iiov,iov)
					}
				  }
				  # find scale and offset
					plotmax=lbnd + diff
					plotmin=lbnd
					ovoff=(plotmin - ovmin*plotmax/ovmax)/(1.0 - ovmin/ovmax)
					ovscale=(plotmax -ovoff)/ovmax
					#write (*,*) "DEBUG: ovmax=",ovmax," ovmin=",ovmin," ovscale=",ovscale," ovoff=",ovoff

				  for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # now scale the data
					if (ovrdat(iiov,iov) > delptup) {   # scale if not deleted point
						ovrdsc(iiov,iov)= ovrdat(iiov,iov) * ovscale + ovoff
					} else {
						ovrdsc(iiov,iov) = delpt   # deleted point
					}
				  }
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(0)   # black
				  }
				}
				if (ovrflg(iov) == 4) {  # autoscaling to plot max, zero stays at zero
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
				  ovmin= 9.99e+33   # overlay min
				  ovmax=-9.99e+33   # overlay max
				  ochan=ovrchn(iov)
				  for (iiov=1; iiov<=ochan; iiov=iiov+1) { # finf min and max in plot range
					if (ovrwav(iiov,iov) >= xmin && ovrwav(iiov,iov) <= xmax) {
						if (ovrdat(iiov,iov) < ovmin && ovrdat(iiov,iov) > delptup) ovmin= ovrdat(iiov,iov)
						if (ovrdat(iiov,iov) > ovmax && ovrdat(iiov,iov) > delptup) ovmax= ovrdat(iiov,iov)
					}
				  }
				  # find scale and offset
					plotmax=lbnd + diff
					plotmin=lbnd
					ovscale=plotmax/ovmax
					#write (*,*) "DEBUG: ovmax=",ovmax," ovmin=",ovmin," ovscale=",ovscale," ovoff=",ovoff

				  for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # now scale the data
					if (ovrdat(iiov,iov) > delptup) {   # scale if not deleted point
						ovrdsc(iiov,iov)= ovrdat(iiov,iov) * ovscale
					}
				  }
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(0)   # black
				  }
				}
			      }
			      # end overlay block


                                #do jjxtmp= 1, nchans {
                                  #write (*,*) "DEBUG: result=", jjxtmp, datac(jjxtmp), dataa(jjxtmp)
                                #}
                                call crtplt(nchans,xmax,xmin,lbnd,diff,datac,dataa,iline)
                                 # call crtin  # DEBUG

				call movabs(0,360*2)
			        call sb(0)
				if (igrmod >= 50 && igrmod <= 53) {
					call xset_color(0)   # black
				}

                                # now compute 3-point band depths

                                # datac = y, dataa= waves
                                xmax = wvmax
                                xmin = wvmin
                                lbnd = bbnd
                                diff = ubnd-lbnd
                                #write (ttyout,*) "DEBUG wriout.r before comp3ptbd"
                                call comp3ptbd (dataa, datac, nchans, xmax, xmin, lbnd, diff)
                                call comptetf  (dataa, datac, nchans, xmax, xmin, lbnd, diff)

                            }   # end of this is graphics modes for screens that can make a plot
                            if (ictrl==iho)
                                go to 910
                            if (iauto==ihb)
                                call lprpct(1,nchans,lbnd,ubnd,dataa)
                            if (iauto==ihb)
                                return

			    if ( igrmod <99 ) {
#
                                call movabs(250*2,0)
                                call sb(0)
                                write(outline,50)
				call gwrite(outline)

#     write wavelength info
				call movabs(0,0)
				call sb(0)
				write(outline,42)itrol(1),itrol(2),nchans
				call gwrite(outline)
#
			    }

			     if ( wmflgmenu == 0 && ovflgmenu == 0 && bdflgmenu == 0 && tetflgmenu == 0) {
							# put default menu on plot window
                                call movabs(0,360*2)
                                call sb(0)
                                write(outline,60)
				call gwrite(outline)
                                write(outline,61)
				call gwrite(outline)
                                write(outline,62)
				call gwrite(outline)
                                write(outline,63)
				call gwrite(outline)
                                write(outline,64)
				call gwrite(outline)
                                write(outline,65)
				call gwrite(outline)
			    }

			     if ( wmflgmenu > 0 || ovflgmenu > 0 || bdflgmenu > 0 || tetflgmenu > 0) {
				call movabs(0,380*2)
				call sb(0)
			     }
			    call pmenus1(wavshift)  # write overlay,
                                                # wavelengths markers
                                                # 3-point band depth
                                                # tetracorder features
                                                # to screen and plot
#
                            j = 1
                            while (j < 80 || igrmod < 99) {
                                if (j>=80)
                                    call what(j)
                                call crtin
                                j = 1
                                call wjfren(j,x,i)

				#write (*,*) "DEBUG: plotit: calling shomenu1"
				call shomenu1(j, i, igo)   # sets what info is shown on the plot:
						# redraw show showov showbd showtet showall showmenu
				#write (*,*) "DEBUG: plotit: after shomenu1, igo=", igo
				if (igo == 9999) {
					go to 9999
				}

				#write (*,*) "DEBUG: plotit: calling defowpt"
				call defowpt (j, i, igo)   # defines
                                                #   this subroutine defines 
                                                #   overlays, wavelength markers,
                                                #   3-point band depths,
                                                #   tetracorder features
                                                #      in the plot routines
				#write (*,*) "DEBUG: plotit: after defowpt, igo=", igo
				if (igo == 9999) {
					go to 9999
				}


                                if (j<80)
                                    go to 900
			    }
			}
900                     continue
#
#     store iopcon in mhistb
#
                        mhistb(1:80) = iopcon
                        il = 0
                        j = 1
                        ji = 1
		    }
#
#     restore iopcon
#
                    iopcon = mhistb(1:80)
                    j = ji
                    call wjfren(j,x,i)
                    ji = j
                    call wjfren(j,x,il)
                    if (iwidok(i) == 1) {     # C,V,W,D, U, or Y: wavelengths
			if (x>0 & x< maxrec) {
				itrol(1) = i
				itrol(2) = x
			} else {
				write (outline,45) itrol(1),itrol(2)
				call gwrite(outline)
			}
		    }
                    if (i==ihb)
                        ibncn2 = ihb
                    if (i==ihb)
                        return
                    if (i==ihh)
                        itrol(3) = ihh
                    if (i==iha)
                        itrol(3) = iha
                    if (i==ihn)
                        itrol(3) = ihn
                    if (i != iha && i != ihn) {
                        if (i!=ihh) {
                            if (i==ihc)
                                go to 920
                            if (i==ihe) {
				ipoint = ihe
                                return
			     }
			    if (i==ihf) {
				ipoint = ihf
				return
			    }
			    if (i==ihp) {
				ipoint = ihp
				return
			    }
			    if (i==ihi) {
				ipoint = ihi
				return
			    }
			    if (i==ihu) {
				ipoint = ihu
				return
			    }
			    if (i==ihv) {
				ipoint = ihv
				return
			    }
			    if (i==ihw) {
				ipoint = ihw
				return
			    }
                            lbnd = bbnd
                            if (i==ihx) {
				ipoint = ihx
                                go to 30
			    }
                            next 1
                        }
		    }
                    if (il!=0)
                        j = j-1
                    if (x>0&&x<maxrec)
                        ji = j
		    call wavlng (itrol(1), itrol(2), ier)
                }
910             continue
                write(outline,80)
		call gwrite(outline)
                call crtin
                ii = 1
                call wjfren(ii,x,irv)
                if (irv!=ihc)
                    go to 930
920         continue
            }
        }
     }
}
930   continue
if (irv==ihe)
    call er
if (irv==ihx)
    ictrl = ihx
return
30  ixit = ihx
return
40  format(4x,a)
%42     format(1x,'wav: ',a1,i5,'  ch=',i5,$)
45  format(1x,'Wavelength specification out of range: ',a,i5)
%50     format(1x,'h=channel,  a=wavelength,  n=energy',$)
60  format(' type: c=Change SCALE')
61  format(' e=EXIT routine, x=EXIT routine WITHOUT WRITING data record')
62  format(' for intarl: f=change weight fraction, i=change intimate spectrum')
63  format('                 p=change pure (second) spectrum')
64  format(' for refcom: u=change dwp of 1st component, v=change 2nd comp')
65  format('                  w=change whole thing')
80  format(1x,'type  c  to Change SCALE, e to ERASE, ','x to EXIT, or return to continue')
end
