subroutine wriout
implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc            this subroutine plots the data on the crt display.
#ccc            added overlays 12/22/2009
#ccc            added wavelength markers 9/2013
#ccc		added wavelength shifts 10/22/2013
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc            scaling,crtpsc,movabs,sb,wavlng,alplty,chplta
#ccc            iwplta,crtplt,sb,movabs,unglic,what,crtin,wjfren
#ccc            dltpts,redhed,lprpct,setspool,chginf,er,pdata,
#ccc            dumpspool
#ccc  argument list description:
#ccc     argumrnts: none
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

integer*4 idlt(SPMAXCHAN)
equivalence (idlt(1),datsc3(1))
real*4 lbnd
character*8 aname
character*7     ovcolor
character*7     wmcolor
character*80 outline        # for X-window writes
character*180 outlinb        # intermediate buffer for X-window writes
#RED
integer*4 iwidok    # function
integer*4 ispcolor  # color
integer*4 iov        #overlay number
integer*4 iwm        #wavelength marker number
integer*4 ochan, iiov
integer*4 il, i, j, itmpo, iero
real*4    ovmin, ovmax, plotmin, plotmax
real*4    wavshift   # wavelength shift (not saved, initialized to zero each time the routine is entered.
integer *4 ipt, ibd, itw
integer*2       chkbit, ibit
real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax
#
#
#     this subroutine plots the data on the crt display
#
#
#     plot times: on TI980B: (for 120 channels, circa 1977)
#                no errors points not connected   7.2 sec.
#                no errors connect points         8.0 sec.
#                errors points not connected    11.0  sec.
#                errors points connected        12.5  sec.
#          times the same for channel, wavelength, energy space
#
#

	wavshift=0.0  # initialize wavelength shift

9999    ji = 80
	#write (ttyout,*) "debug: wriout after label 9999"
	i = 0
	ibncn2 = 0
	ixit = 0

#
#  find line type
#
if (idad!=2)
    do i = 1,nchans
        error(i) = 0.0
if (ictrl!=ihi) {        # if != ihi, then regular plot, if ihi, then go to info change routines
    if (ictrl!=iho) {    # not overlay more, so erase the x-window
        call er
    }
    if (iauto!=ihb) {
        if (iauto!=iha & iauto!=ihca & iauto!=ihcb) {
            igo = 8
            go to 10
        }
    }
#
#   if the file is a text file, then we do not want to do the plot
#
    ibit = 1
    if (chkbit(icflag,ibit) == 1) {     # text data record
	write (ttyout,*) "Text bit set, so skipping plot"
	inxt = ihe
	return
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
    if (inxt==ihx) go to 30
    if (inxt==ihe) {
	 #write (ttyout,*) "debug: wriout inxt==ihe, return"
	return
    }
    if (inxt==ihg) go to 9999
    if (inxt!=ihr) {
        repeat {     #     calculate parameters for the plot
            lbnd = bbnd
            diff = ubnd-lbnd
            if (diff>0.1e-15) go to 20
            repeat {
               igo = 6
10             call crtpsc(igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i,dataa,iauto)
               if (igo!=4) break 1
20             ig = 1
               repeat {
                    if (ji>=80) {
                        if (ictrl!=iho) call er
                        if (ig==0) ig = 1
                        repeat {
                            call wavlng (itrol(1), itrol(2), ier)  # reads waves into dataa

				do iwj = 1, SPMAXCHAN {
					dataa(iwj) = dataa(iwj) + wavshift   # wavelengths + shift
				}
                            if (igrmod >= 99) {   # make a simple ascii plot
                                call lpcrtp(1,nchans,lbnd,ubnd,dataa)
			    }
                            if (igrmod < 99) {    # this is graphics modes for screens that can make a plot
#
#     write  identification to the plot
#
                                call movabs(0,590)
                                call sb(0)
                                call namdev (idv1,aname)
                                write(outline,40)ititl,idv1,ifl1,aname
                                call gwrite(outline)
                                write(outline,41)ihist
                                call gwrite(outline)

#
#     draw the plot
#
                                if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				}
                                call alplty(lbnd,ubnd)        # draws the box and tick marks on vert axis
                                diff = ubnd-lbnd
                                if (itrol(3)==iha)
                                    call wvplta(nchans,wvmax,wvmin,iline) # draw horizontal axis ticks and labels
									  # and wavelength marks
                                if (itrol(3)==ihn)
                                    call iwplta(nchans,wvmax,wvmin,iline) # draw horizontal axis ticks and labels
									  # for inverse wavelengths
									  # need to add wavelength marks
                                xmax = wvmax
                                xmin = wvmin
                                if (itrol(3)!=iha&&itrol(3)!=ihn)
                                    call chplta(nchans,xmax,xmin,dataa,iline)  # draw horizontal ticks in channel space

			      # overlay block
			      for (iov=1; iov<=6; iov=iov+1) {    # overlays
				if (ovrflg(iov) == 1) {  # straight plot, no autoscaling
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdat(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
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
#XWIN					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				  }
				}
				if (ovrflg(iov) == 3) {  # autoscaling to plot min and max
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
#XWIN					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
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
#XWIN					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				  }
				}
			      }   # end overlay block

                                if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(spcolor)   # usr selected color
				}
				#write (ttyout,*) "DEBUG wriout.r before crtplt call"
                                call crtplt(nchans,xmax,xmin,lbnd,diff,datac,dataa,iline)
                                call movabs(0,720)
                                call sb(0)
                                if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
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

                            }  # end of this is graphics modes for screens that can make a plot
                            if (ictrl==iho) go to 920

			    # iauto = auto scale the plot flag
                            if (iauto==ihb) {
                                call lprpct(1,nchans,lbnd,ubnd,dataa)
			    }
                            if (iauto==ihb) {
				write (ttyout,*) "DEBUG wriout.r iauto==ihb, return is next"
                                return
			    }
#
#     call glich removal routine
#
                            if (ig==ihg) {
                                call unglic(ig)
			    }
                            if (ig==ihg) go to 910
                            if (ig==0) go to 910

                            if ( igrmod <99 ) {
#
#                               write wavelength info
#
                                call movabs(0,6)
                                call sb(0)
                                if (igrmod >= 50 && igrmod <= 53) {
			    	  if ( wmflgmenu == 0 && ovflgmenu == 0 && bdflgmenu == 0 && tetflgmenu == 0) {
                                    write(outline,43)itrol(1),itrol(2),nchans
				  } else {
                                    write(outline,431)itrol(1),itrol(2),nchans
				  }
                                } else {
                                    write(outline,42)itrol(1),itrol(2),nchans
                                }
                                call gwrite(outline)
                            }
                            call movabs(0,760)
                            call sb(0)
# write user menu
			    if ( wmflgmenu == 0 && ovflgmenu == 0 && bdflgmenu == 0 && tetflgmenu == 0) {
							# put default menu on plot window
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
			    }
#ZZZZZZZZZZZZZZZ
			    call pmenus1(wavshift)  # write overlay,
						# wavelengths markers
						# 3-point band depth
						# tetracorder features
						# to screen and plot
#ZZZZZZZZZZZZZZ

#  User input decoding:
                            j = 1
				#write (ttyout,*) "DEBUG wriout.r just before user input"
                            while (j < 80 || igrmod < 99) {
                                if (j>=80) call what(j)
				#write (ttyout,*) "DEBUG wriout.r just before crtin"
                                call crtin
                                j = 1
                                call wjfren(j,x,il)

				call shomenu1(j, il, igo)   # sets what info is shown on the plot:
						# redraw show showov showbd showtet showall showmenu
				if (igo == 9999) {
					go to 9999
				}
				
				#write (ttyout,*) "DEBUG wriout.r check for wavelength shift"
				if (j < 50 && il == ihw) {   # check for wavelength shift
					if (iopcon(j-1:j+7) == "wavshift=") {
						j=j+8
						call wjfren(j,x,il)
						if ( il == 0 ) {
							wavshift=x
							#write(*,*) "debug: wavshift=", wavshift
							call what(j)
							go to 9999
						} else {
							call what(j)
							write(ttyout,*) "Error finding wavshift"
							go to 9999
						}
					}
				}
				if (j < 70 && i == ihcl) {   # capital L: change line color
					call wjfren(j,x,il)
					ispcolor=x+0.5
					if (ispcolor < 0.0 || ispcolor > 9.0) {

						write (outline,*) "Line color out of range"
						call what(j)
						go to 9999
					}
					spcolor=ispcolor   # set line color
				}

# ZZZZZZ define
				call defowpt (j, il, igo)   # defines
						#   this subroutine defines 
						#   overlays, wavemength markers,
						#   3-point band depths,
						#   tetracorder features
						#      in the plot routines
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
                    if (i==ihb) {
                        ibncn2 = ihb
			write (ttyout,*) "DEBUG wriout.r i==ihb, ibncn2 = ihb, return"
                        return
                    }
                    if (i==ihh || i == iha || i == ihn) itrol(3) = i
                    if (i!=iha&&i!=ihn) {
                        if (i!=ihh) {
                            if (i==ihg) {
                                ig = ihg
                                next 1
                            }
                            if (i==ihc) go to 930
                            if (i==ihcg ) {   # graphics cursor position
                                call serase(0,620,1022,736)
                                call movabs (0,718)
                                call sb(0)
                                write (outline,72)
                                call gwrite(outline)
                                if (igrmod >= 50 && igrmod <= 53) {
                                    write (outline,74)
                                } else {
                                    write (outline,73)
                                }
			        call gwrite(outline)
				iier = 0
				while(iier != ihe && iier != ihx && iier != -1) {
					call gcrpos(iix,iiy,txpos,typos,
						xmax,xmin,lbnd,diff,iopcon,iier)
					call gcchan(iix,iiy,txpos,typos,
						imatch,nchans,dataa,datac,iier)
				}
                                next 1
                            }
                            if (i==ihcs) {
				bbnd = lbnd
                                call window(bbnd,ubnd,xmin,xmax,diff)
                                wmina=xmin
                                wmaxa=xmax
				lbnd = bbnd
                            }
                            if (i==ihca) {    # band analysis
                                call bdanal(lbnd,diff,xmax,xmin,wavshift)
                            }
                            if (i==ihe) {
                                call movabs(0,720)
                                call sb(0)
				#write (ttyout,*) "DEBUG wriout.r i==ihe return"
                                return
                            }
                            lbnd = bbnd
                            if (i==ihi) go to 940
                            if (i==ihl) {
                                iline = x
                                if (x<0||x>10) iline = 0
                                next 1
                            } else if (i==ihr) {
                                call dltpts(ji,ijk,idlt,4852,ic)
                                if (ijk==0&&ic==ihc) ji = 80
                                if (ijk>0) {
                                    do kk = 1,ijk {
                                        datac(idlt(kk)) = delpt
                                        data(idlt(kk)) = delpt
                                    }
                                }
                                next 1
                            } else if (i==ihp && il==ihd) {
                                lpline = 60
                                call setspo
                                call pdata(0,1,1,1,filno,lpline)
                                call dumpsp
                                next 1
                            } else {
#
#     call the line printer plot routine
#
                                if (i==ihp) {
                                    call wavlng (itrol(1), itrol(2), ier)
					do iwj = 1, SPMAXCHAN {
						dataa(iwj) = dataa(iwj) + wavshift   # wavelengths + shift
					}
                                    j = 1
                                    if (x>10) x = 10
                                    if (x>0) j = x
                                    do ij = 1,j
                                    call lprpct(1,nchans,lbnd,ubnd,dataa)
                                }
                                if (i==ihx) go to 30
                                next 1
                            }
                        }
                    }
910                 continue
                    if (il != 0) j = j-1
                    if (x>0 && x<maxrec) ji = j
                    call wavlng (itrol(1), itrol(2), ier)
			do iwj = 1, SPMAXCHAN {
				dataa(iwj) = dataa(iwj) + wavshift   # wavelengths + shift
			}
                }
920             continue
                write(outline,80)
                call gwrite(outline)

		#write (ttyout,*) "DEBUG wriout.r just before crtin below 920"
                call crtin
                ii = 1
                call wjfren(ii,x,irv)
                if (irv!=ihc) go to 950
930             continue
            }
        }
940     continue
    }
}
950   continue
if (irv==ihe) call er
if (irv==ihx) ictrl = ihx
#write (ttyout,*) "DEBUG wriout.r after statement label 950"
return

30  ixit = ihx
    #write (ttyout,*) "DEBUG wriout.r after statement label 950"
    return

# RED Added SOLARIS and SUNOS below

40  format(4x,a,2x,'file= ',a,i5,' (',a,')')
41  format(4x,'history=',a)
#NONHPUX%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#NONHPUX%     C 'h=channel, a=wave, n=energy',$)
#SOLARIS%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#SOLARIS%     C 'h=channel, a=wave, n=energy',$)
#SUNOS%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#SUNOS%     C 'h=channel, a=wave, n=energy',$)
#DEC%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#DEC%     C 'h=channel, a=wave, n=energy',$)
#HPUX%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#HPUX%     C 'h=channel, a=wave, n=energy',$)
#LINUX%42    format(' wav: ',a1,i5,'  ch=',i6,15x,'h=channel, a=wave, n=energy',$)
#IA64HPUX%42        format(' wav: ',a1,i5,'  ch=',i6,15x,
#IA64HPUX%     'h=channel, a=wave, n=energy',$)

43  format(' wav: ',a1,i5,'  ch=',i5,5x,'h=channel,  a=wavelength,  n=energy')
431 format(' wav: ',a1,i5,'  ch=',i5)
45  format(1x,'Wavelength specification out of range: ',a,i5)
60  format(' type: c=Change SCALE; show;  showov; showwm; showall; showmenu; redraw')
61  format(5x,'e=EXIT routine;  g=Remove GLITCHES  l+number=LINE TYPE   Ln=line color n')
62  format(5x,'r+channels+c=DELETE Channels;   A=Interactive BAND ANALYSIS')
63  format(5x,'p=LPrinter PLOT; i=Change other INFO;   G=read Graphics CURSOR pos.')
64  format(4x,' pd=PRINT data;   x=EXIT routine WITHOUT WRITING data record')
72  format(26(' '),'Graphics Cursor Read Routine')
73  format(12(' '),'to enter Graphics Cursor Pos: <cr> or e to exit')
74  format(5x,'Use left mouse button to pick position or right mouse button to exit')
80  format(1x,'type  c  to Change SCALE, e to ERASE, x to EXIT, or return to continue')
end
