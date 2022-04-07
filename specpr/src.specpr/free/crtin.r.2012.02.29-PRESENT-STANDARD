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
	include "../common/inputhistory"
#RED
	integer*4 ihchar    # function ihchar
	integer*4 varpar    # function varpar
	integer*4 lnb       # function lnb
	integer*4 iov       # overlay number

	character*1     irespo, chtmp, ihbcksl, rbrakt
	character*80	txtbuf, ipxtxt
	character*7     ovcolor
	logical         ixit,savflg,keycon
	integer*4 varsub, idummy, ipxflg, ipxstr, ipxend, cmdverbose
	integer*4 ii, iij, iik
	integer*4 idlt(4852), ipts, itmpch

	ihbcksl = char(92)  # this is the backslash character
	rbrakt  = char(123) # right curly bracket

#RED Initialize to 1
	ipxstr = 1

	if (icoman<=0 | icoman>20) icoman = 1

	ipxflg = 0   # no pixel coordinate processing (yet)

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
		if (inchar < 74 && ifchar == iho) {

                        # overlays on:

			iov=0
			if (iopcon(inchar-1:inchar+4) == 'ov1=on') { #overlay1
				iov=1
			}
			if (iopcon(inchar-1:inchar+4) == 'ov2=on') { #overlay2
				iov=2
			}
			if (iopcon(inchar-1:inchar+4) == 'ov3=on') { #overlay3
				iov=3
			}
			if (iopcon(inchar-1:inchar+4) == 'ov4=on') { #overlay3
				iov=4
				go to 1000
			}
			if (iopcon(inchar-1:inchar+4) == 'ov5=on') { #overlay3
				iov=5
			}
			if (iopcon(inchar-1:inchar+4) == 'ov6=on') { #overlay3
				iov=6
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
			}
			if (iov > 0) {

				ovrflg(iov)=ovrflgb(iov)

				# now check for a chane in option

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
					write (*,*) "DEBUG: deleting overlay channels"
					call dltpts(inchar, ipts, idlt, itmpch, ifchar)
					write (*,*) "DEBUG: deleting ", ipts, "  overlay channels"
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
			if (iopcon(inchar-1:inchar+5) == 'ov4=off') { #overlay3
				ovrflg(4)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov5=off') { #overlay3
				ovrflg(5)=0
				go to 1000
			}
			if (iopcon(inchar-1:inchar+5) == 'ov6=off') { #overlay3
				ovrflg(6)=0
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
                        write (ttyout,*) "tty input error:", ier
                        write (ttyout,*) "line:", iopcon
                        write (ttyout,*) "closing current input and re-opening tty"
                        close(pipe(pipelv))
                        open(5,file=TTY)
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
