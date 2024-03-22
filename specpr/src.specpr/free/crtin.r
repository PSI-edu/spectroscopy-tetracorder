	subroutine crtin
	implicit integer*4 (i-n)

#ccc  version date: @(#)crtin.r	2.25 12/13/89 08:55:41
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine reads one line from the
#ccc         terminal and returns it in iopcon, if the first
#ccc         character is <,!,?,$,%, or > this is a command
#ccc         of some type and it is handled appropriately.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          wjfren,list,typein,xfer,copyin,system,refile
#ccc  argument list description: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#####################################################################
############### new version of crtin using FORTRAN-77 ###############
#####################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/filenames"
	include "../common/key1"
	include "../common/ioftyp"
	include "../common/hptrm"
	include "../common/pipes"
	include "../common/iocontrol"
	include "../common/overlys"
	include "../common/sp3pfeat"
	include "../common/tetfeat"
	include "../common/wavemarks"
	include "../common/inputhistory"
#RED
	integer*4 ihchar    # function ihchar
	integer*4 varpar    # function varpar
	integer*4 lnb       # function lnb
	integer*4 iov       # overlay number
	integer*4 iwm       # wavelength marker number

	character*1     irespo, chtmp, ihbcksl, rbrakt
	character*80	txtbuf, ipxtxt
	character*7     ovcolor
	character*7     wmcolor
	logical         ixit,savflg,keycon
	integer*4 varsub, idummy, ipxflg, ipxstr, ipxend, cmdverbose
	integer*4 ii, iij, iik
	integer*4 idlt(SPMAXCHAN), ipts, itmpch

	ihbcksl = char(92)  # this is the backslash character
	rbrakt  = char(123) # right curly bracket

#RED Initialize to 1
	ipxstr = 1

	if (icoman<=0 | icoman>20) icoman = 1

	ipxflg = 0   # no pixel coordinate processing (yet)
	ier=0

999     icontin = 1  # begin repeat block
	repeat {
		#write (*,*) 'DEBUG: crtin start repeat loop'
    #########################################################
    ##### read input line from terminal or command file #####
    ########## or get next logical line from copcon #########
    #########################################################
1000	if (cndx==0) {
			if (!redire) {
				write(ttyout,'(a1,$)') iprom
				#write(ttyout,'("debug: at prompt pipelv=",i6)') pipelv
				ioutverbose = 0  # if not redirecting
						# output, then output
						# all user prompts
			} else {
				if (inline>iend) goto 2000
				inline = inline + 1
			}
#
# inpipe is a pointer to an array of integers which specify the
# unit number of additional files to be read from.  This stuff is
# all managed by the END case on this read, and the subroutine refile
#
			inpipe = pipe(pipelv)
			read(inpipe,1,end=2000,err=2000,iostat=ier) iopcon
			#write (*,*) 'DEBUG: iopcon=', iopcon
			savflg = .true.
			ilast = lnb(iopcon)

			
			if (inhsenbl == 1) {
				# save input into history
				if (inhistch+ilast+1 > 19408 ) {
					write(ttyout,"(' ERROR: input history recording over limit',/)")
					inhsenbl=0
				} else {
					tinphist(inhistch:inhistch+ilast)=iopcon(1:ilast)
					inhistch=inhistch+ilast+1
					tinphist(inhistch:inhistch)=char(10)  # line feed
					inhistch=inhistch+1                   # next position in the history

					write(ttyout,*) "         recording history, inhistch=", inhistch
				}

			}


			if (copy) {
				write(cpylun,1) iopcon(1:ilast)
			}
			if (redire) {
				if (ioutverbose == 0) {
					write(ttyout,4) iopcon(1:ilast)
				}
			}
		} else {
			iopcon = copcon(2:80)
			#write (*,*) 'DEBUG crtin 12'
			ilast = lnb(iopcon)
			if (ioutverbose == 0) {
				write(ttyout,4) iopcon(1:ilast)
			}
		}
#
# $ substitution is done
#
10		if (index(iopcon,'$')!=0 & varsub(idummy)==-1) {
			write(ttyout,2)           #line overflow condition
			write(ttyout,'(a1,$)') iprom
			if (!redire) {
				read(ttyin,1,end=1000,err=1000) irespo
				if (irespo=='r') next
			} else goto 2000
		}
#
# Value variable substitution
#
		ivv = index(iopcon,rbrakt)
		if (ivv > 1) {
			chtmp = iopcon(ivv-1:ivv-1)
		} else {
			chtmp = ' '
		}
		if (chtmp == ihbcksl) {  # erase back slash
			iopcon(ivv-1:79) = iopcon(ivv:80)
			iopcon(80:80) = ' '
		}
		if ((ivv > 0) & (chtmp != ihbcksl)) {
			itmp = varpar(idummy)
			if (itmp == -1) {
				write(ttyout,*) 'Error in variable substitution'
				goto 2000
			} else if (itmp ==1) {
# Just a set.
				ixit = .false.
			}

		}
#
# ; processing done
#
		inchar=1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 1: i=',inchar
		if (inchar > 80) inchar=80
		if ((ifchar != ihchar('='))|(iopcon(inchar:inchar)!='=')
				&(inchar < 80)) { 
			cndx = index(iopcon,';')      #not alias definition
			if (cndx!=0) {
				copcon = iopcon(cndx:80)
				iopcon(cndx:80) = ' '
			}
		}
#
# pixel processing done on 3d files
#
		ipxflg = 0   # no pixel coordinate processing (yet)
		ipx = index(iopcon,'p')
		if (ipx > 1 && ipx < 77) {
			if (iopcon(ipx:ipx+2) == 'px(' &
				iopcon(ipx-1:ipx-1) != ihbcksl ) { # found pixel
				ipxtxt = iopcon            # save original text
                                                           # and its not escaped
				ipxstr = ipx               # save start position
				call prpixl (ipx,ipxend,igo)    # process pixel
				if (igo == 1000) go to 1000 # error
				ipxflg = 1    # pixel coords have been processed
			}
		}
#
############# pixel processing complete
#
#
############ overlay on/off processing
#

		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 2: i=',inchar
		if (inchar < 74 && ifchar == iho && iopcon(inchar:inchar) == "v") {

                        # overlays on:
			#              ovrflgb is the saved state of the overlay

			iov=0
			if (iopcon(inchar-1:inchar+4) == 'ov1=on') { #overlay1
				iov=1
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov2=on') { #overlay2
				iov=2
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov3=on') { #overlay3
				iov=3
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov4=on') { #overlay4
				iov=4
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov5=on') { #overlay5
				iov=5
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov6=on') { #overlay6
				iov=6
				ovrflg(iov) = ovrflgb(iov)
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ovlist') {
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
1234				format(1x,'ov', i1,'=',a1,i6,' ',a1,i6,' ',a,' ',a,' ',a,i7)
				go to 1000
			}
			if (iov > 0) {

				ovrflg(iov)=ovrflgb(iov)

				# now check for a change in option

				inchar=inchar+5
				call wjfren(inchar,x,ifchar)
				if (ifchar == ihca) {    # A = autoscale
					
					ovrflg(iov) = 3   # overlay with autoscale to min max
					ovrflgb(iov) = 3
					if (iopcon(inchar:inchar) == "2") {   # overlay with autoscale 2% margin

						ovrflg(iov) = 2
						ovrflgb(iov) = 2
					}
					if (iopcon(inchar:inchar) == "0") {   # overlay with autoscale to max, min stays as original

						ovrflg(iov) = 4
						ovrflgb(iov) = 4
					}
					if (ovrflg(iov) == 2 || ovrflg(iov) == 4) {
						inchar= inchar + 1
					}
				} else if (ifchar == ihcn) {   # turn off autoscale

					ovrflg(iov) =  1  # no auto scaling
					ovrflgb(iov) = 1

				} else if (ifchar == ihd) {   # delete points
					itmpch=ovrchn(iov)
					call dltpts(inchar, ipts, idlt, itmpch, ifchar)
					if (ifchar==ihe) {
						go to 1000
					} else if (ifchar==ihx) {
						go to 1000
					}
					if (ipts > 0 && iov > 0) {
						for(iik=1; iik<=ipts; iik=iik+1) {
							ovrdat(idlt(iik),iov) = -1.23e34   # deleted point
						}
					}
				}
				if (ifchar == ihd) {
					itmpch=ovrchn(iov)
					#write (*,*) "DEBUG: deleting overlay channels"
					call dltpts(inchar, ipts, idlt, itmpch, ifchar)
					#write (*,*) "DEBUG: deleting ", ipts, "  overlay channels"
					if (ifchar==ihe) {
						go to 1000
					} else if (ifchar==ihx) {
						go to 1000
					}
					if (ipts > 0 && iov > 0) {
						for(iik=1; iik<=ipts; iik=iik+1) {
							ovrdat(idlt(iik),iov) = -1.23e34   # deleted point
						}
					}
				}
				go to 1000
			}


                        # overlays off:

			if (iopcon(inchar-1:inchar+5) == 'ov1=off') { #overlay1
				ovrflg(1)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov2=off') { #overlay2
				ovrflg(2)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov3=off') { #overlay3
				ovrflg(3)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov4=off') { #overlay4
				ovrflg(4)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov5=off') { #overlay5
				ovrflg(5)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov6=off') { #overlay6
				ovrflg(6)=0
				go to 1000
			}
		}

############ wavelength marker on/off processing
#
		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 2: i=',inchar
		if (inchar < 74 && ifchar == ihw  && iopcon(inchar:inchar) == "m") {

                        # wavelength markers on:

			iwm=0
			if (iopcon(inchar-1:inchar+4) == 'wm1=on') { # wavelength mark 1
				iwm=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wm2=on') { # wavelength mark 2
				iwm=2
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wm3=on') { # wavelength mark 3
				iwm=3
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wm4=on') { # wavelength mark 4
				iwm=4
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wm5=on') { # wavelength mark 5
				iwm=5
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wm6=on') { # wavelength mark 6
				iwm=6
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'wmlist') {
				for (iwm=1; iwm<=6; iwm=iwm+1) {
					if (wmrflg(iwm) > 0) {

						if (iwm == 1) wmcolor='red    '
						if (iwm == 2) wmcolor='blue   '
						if (iwm == 3) wmcolor='green  '
						if (iwm == 4) wmcolor='orange '
						if (iwm == 5) wmcolor='cyan   '
						if (iwm == 6) wmcolor='magenta'
						write (ttyout,1235) iwm, 
							wmops(iwm), wmcolor,
							wmrwav(1:wmnum(iwm),iwm)
					}
				}
1235				format(1x,'wm', i1,'=',a4,' ',a7,' ',6f12.5)
			}
			if (iwm > 0) {

				wmrflg(iwm)=wmrflgb(iwm)
				go to 1000
			}


                        # wavelength marks off:

			if (iopcon(inchar-1:inchar+5) == 'wm1=off') { #wavelength mark 1
				wmrflg(1)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'wm2=off') { #wavelength mark 2
				wmrflg(2)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'wm3=off') { #wavelength mark 3
				wmrflg(3)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'wm4=off') { #wavelength mark 4
				wmrflg(4)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'wm5=off') { #wavelength mark 5
				wmrflg(5)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'wm6=off') { #wavelength mark 6
				ovrflg(6)=0
				go to 1000
			}
		}

############ 3pt band depths on/off processing
#
		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 2: i=',inchar
		if (inchar < 74 && ifchar == ihb  && iopcon(inchar:inchar) == "d") {

                        # 3pt band depths:

			ibd=0
			if (iopcon(inchar-1:inchar+4) == 'bd1=on') { # 3pt band depth 1
				sonoff(1)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd2=on') { # 3pt band depth 2
				sonoff(2)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd3=on') { # 3pt band depth 3
				sonoff(3)=1
				#write(*,*) "debug: crtin: bd3=on"
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd4=on') { # 3pt band depth 4
				sonoff(4)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd5=on') { # 3pt band depth 5
				sonoff(5)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd6=on') { # 3pt band depth 6
				sonoff(6)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd7=on') { # 3pt band depth 7
				sonoff(7)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd8=on') { # 3pt band depth 8
				sonoff(8)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bd9=on') { # 3pt band depth 9
				sonoff(9)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'bdlist') {
				for (ibd=1; ibd<=imax3pt; ibd=ibd+1) {
					if (sfmode(ibd) > 0) {

						write (ttyout,1237) ibd,
							sfeatname(ibd),
							sbdepth(ibd)
					}
				}
1237				format(1x,'bd', i1,'=',a,'  ' ,f8.4)
				go to 1000
			}


                        # 3pt band depths off:

			if (iopcon(inchar-1:inchar+5) == 'bd1=off') { # 3pt band depth 1
				sonoff(1)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd2=off') { # 3pt band depth 2
				sonoff(2)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd3=off') { # 3pt band depth 3
				sonoff(3)=0
				#write(*,*) "debug: crtin: bd3=off"
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd4=off') { # 3pt band depth 4
				sonoff(4)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd5=off') { # 3pt band depth 5
				sonoff(5)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd6=off') { # 3pt band depth 6
				sonoff(6)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd7=off') { # 3pt band depth 7
				sonoff(7)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd8=off') { # 3pt band depth 8
				sonoff(8)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'bd9=off') { # 3pt band depth 9
				sonoff(9)=0
				go to 1000
			}
		}

############ tetracorder feature on/off processing
#
		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 2: i=',inchar
		if (inchar < 74 && ifchar == iht  && iopcon(inchar:inchar) == "f") {

                        # tetracorder feature on

			ibd=0
			if (iopcon(inchar-1:inchar+4) == 'tf1=on') { # tetracorder feature 1
				tetonoff(1)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf2=on') { # tetracorder feature 2
				tetonoff(2)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf3=on') { # tetracorder feature 3
				tetonoff(3)=1
				#write(*,*) "debug: crtin: tf3=on"
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf4=on') { # tetracorder feature 4
				tetonoff(4)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf5=on') { # tetracorder feature 5
				tetonoff(5)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf6=on') { # tetracorder feature 6
				tetonoff(6)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf7=on') { # tetracorder feature 7
				tetonoff(7)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf8=on') { # tetracorder feature 8
				tetonoff(8)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tf9=on') { # tetracorder feature 9
				tetonoff(9)=1
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'tflist') {
				for (itf=1; itf<=imaxtet; itf=itf+1) {
					if (tfmode(itf) > 0) {

					    # write (*,*) "DEBUG: tfmode",itf,"= ",tfmode(itf)

					    if (tfcont(itf) == 1) {            # linear
						write (ttyout,1238) itf,
							tfeatname(itf),
							tetfna(itf),
							tetfnc(itf),
							tleftwave(1,itf),
							tleftwave(2,itf),
							trightwave(1,itf),
							trightwave(2,itf),
							tbdepth(itf)

					    } else if (tfcont(itf) == 2) {              # curved
						write (ttyout,1239) itf,
							tfeatname(itf),
							tetfna(itf),
							tetfnc(itf),
							tleftcwave(1,itf),
							tleftcwave(2,itf),
							tleftwave(1,itf),
							tleftwave(2,itf),
							trightwave(1,itf),
							trightwave(2,itf),
							trightcwave(1,itf),
							trightcwave(2,itf),
							tbdepth(itf)
					    } else {

						write (ttyout,*) "Error: tetracorder continuum mode, not L or C"
						write (ttyout,*) "        tfcont(",itf,") = ", tfcont(itf)
						call what(0)
					    }
					}
				}
1238	format(1x,'tf', i1,'=',1x,a,1x, a4,1x, a4,
			f7.4,1x,f7.4,1x,f7.4,1x,f7.4,1x, f8.4)
1239	format(1x,'tf', i1,'=',1x,a,1x, a4,1x, a4,
		f7.4,1x,f7.4,1x,f7.4,1x,f7.4,1x,f7.4,
                  1x,f7.4,1x,f7.4,1x,f7.4,1x, f8.4)

				go to 1000
			}


                        # tetracorder feature off:

			if (iopcon(inchar-1:inchar+5) == 'tf1=off') { # tetracorder feature 1
				tetonoff(1)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf2=off') { # tetracorder feature 2
				tetonoff(2)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf3=off') { # tetracorder feature 3
				tetonoff(3)=0
				#write(*,*) "debug: crtin: tf3=off"
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf4=off') { # tetracorder feature 4
				tetonoff(4)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf5=off') { # tetracorder feature 5
				tetonoff(5)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf6=off') { # tetracorder feature 6
				tetonoff(6)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf7=off') { # tetracorder feature 7
				tetonoff(7)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf8=off') { # tetracorder feature 8
				tetonoff(8)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'tf9=off') { # tetracorder feature 9
				tetonoff(9)=0
				go to 1000
			}
		}


#
############ help processing
#
		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 2: i=',inchar
		if (inchar < 78 ) {
			if (iopcon(inchar:inchar+2) == 'elp') { #help requested
				call sphelp (iopcon)
				go to 1000
			}
		}

#
############ user prompt verbose level processing done
#
#               need: CMD crt verbose=n, n = 0, 1, 2, 3, 4 , 5

		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 3: i=',inchar
		if (inchar < 73 & ifchar == ihcc) {
			if (iopcon(inchar:inchar+14) == 'MD crt verbose=') { 
				inchar= inchar+15
				call wjfren(inchar,x,ifchar)
				#write (*,*) 'DEBUG: in crtin: wjfren 4: i=',inchar
				ii = x +0.5
				itmp = cmdverbose(ii)
				#write (ttyout,*) 'DEBUG: ioutverbose=',itmp, ioutverbose

				cndx = 0
				ixit = .false.
				go to 1000
			}
		}


# other special character processing

		inchar = 1
		call wjfren(inchar,x,ifchar)
		#write (*,*) 'DEBUG: in crtin: wjfren 5: i=',inchar
		ixit = .true.


	###############################
	###### list command file ######
	###############################
		if (ifchar == ihchar('?')) {
			call list (inchar)
			go to 1000           # next
		}

      ################################################################
      #####   function is to directly type in permanent command  #####
      #####   or do alias functions.                             #####
      ################################################################
		if (ifchar == ihchar('=')) {
			ier = 0
			if (iopcon(inchar:inchar) == '=') { #alias definition
				call setkey
				cndx = 0
			} else {                            #define $variable
				call typein (inchar,ier)
			}
			if (ier!=0) go to 1000           # next
			ixit = .false.
		}

########################################################################
###   function is to transfer rolling command to permanent command   ###
########################################################################
		if (ifchar == ihchar('%')) {
			call xfer (inchar,ier)
			if (ier!=0) {
				go to 1000           # next
			}
			ixit = .false.
		}


	#########################################################
	#########    copy input to an output file       #########
	#########################################################
		if (ifchar == ihchar('>')) {
			call copyin (inchar)
			ixit = .false.
		}

	  ####################################################
	  #####   function is to execute shell command   #####
	  ####################################################
		if (ifchar == ihchar('!')) {
			call keytrn (keycon)  # translate any aliases
			call system(iopcon(inchar:80) // char(0))
			ixit = .false.
			write(ttyout,'("!",/,a1,$)') iprom
		}

	#########################################################
	#######         redirect input from file        #########
	#########################################################
		if (ifchar == ihchar('<')) {
			call refile (inchar,ier)
			if (ier!=0) go to 1000           # next
			ixit = .false.
		}

##############################################################
#*********************  just return line *********************
##############################################################

		#write (*,*) 'DEBUG: crtin: end of repeat loop'

		#} # end of repeat here or much below?

		if (savflg) {
			ilast = lnb(iopcon)
			if (cndx==0) {
				txtbuf = iopcon
				if (ipxflg == 1) {    # restore pixel coords
					if (ipxstr >= 1 & ipxend <= 80
						& ipxstr <= ipxend) {
						txtbuf(ipxstr:ipxend) =
							ipxtxt(ipxstr:ipxend)
						}
				}
				write(cmdlun,rec=icoman) txtbuf(1:80)
			} else if (cndx==1) {
				write(cmdlun,rec=icoman) copcon(1:80)
			} else {
				txtbuf = iopcon
				if (ipxflg == 1) {    # restore pixel coords
					if (ipxstr >= 1 & ipxend <= cndx-1
						& ipxstr <= ipxend) {
						txtbuf(ipxstr:ipxend) =
							ipxtxt(ipxstr:ipxend)
						}
				}
				write(cmdlun,rec=icoman) txtbuf(1:cndx-1),
							copcon(1:80-cndx)
			}
			icoman = icoman + 1
			if (icoman >20) icoman = 1
			savflg = .false.
		}

#
# alias substitution done
#
		if (ixit) {
			keycon = .false.
			call keytrn (keycon)
			if (keycon) {  #check for cases(==,=,%,<,>,!)
					#in translated string
				if (inchar > 1) inchar=inchar - 1
                       		ixit = .false.
				go to 10    
			}
		}


		#write (*,*) 'DEBUG: in crtin ibefore ixit block 2'
		if (ixit) { 

			#write (*,*) 'DEBUG: in crtin return block 1'
			ii = 1
			while (ii < 80) {
				#write (*,*) 'DEBUG: in crtin return block 2'
				if (iopcon(ii:ii+1) == ihbcksl //'#') { #comment
					if (ii == 1) go to 1000  # read new line
					if (ii > 1 &
						iopcon(ii-1:ii-1) != ihbcksl) {
#
#							if no backslash to
#							escape the \# then
#							remove comment.
#
						do i = ii, 80 {
							iopcon(i:i) = ' '
						}
					}
					#write (*,*) 'DEBUG: leaving crtin 1'
					return
				}
				ii = ii +1
			}
			#write (*,*) 'DEBUG: leaving crtin 2'
			return
		}
	}

# close command redirection from files.

# note if the following section is modified, a similar section
#      in the subriutine "what" may also need to be changed.

2000	if (pipelv == 1) {
                if (ier > 0) {
                        write (ttyout,*) "tty input error on pipe:", ier
                        write (ttyout,*) "line:", iopcon
			if (ier > 0) {
				write (ttyout,*) "maybe the program",
					" feeding the pipe crashed",
					" or did not start."
			}
                   if (ier < 4000) {
			# 10/2018: Linux mint 19 generates an error from the pipe
			# input stream when java starts.
			# The error is in the 9000-22000 range but seems benign,
			# so ignore the high value input error messages.

                        write (ttyout,*) "closing current input and re-opening tty"
                        close(pipe(pipelv))
                        open(5,file=TTY)
		   }
                }
		redire = .false.
	} else {
		close(pipe(pipelv))
		pipelv = pipelv -1
		if (pipelv == 1) redire = .false.
	}
	goto 1000

1	format (a)
2	format (' LINE OVERFLOW, Return to continue, r to REENTER input')
%3	format (1x,a1,$)
4	format (1x,a)
%5	format (' INVALID PIXEL SPECIFICATION, reenter line',/,a1,$)
	end
