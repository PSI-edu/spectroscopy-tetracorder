	subroutine f14(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine edits data and error files.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    rstart,hreset,crtin,wjfren,filinp,chdata,namdev
#ccc                    er,chdel,chinst,chlist,chprnt
#ccc  argument list description:
#ccc      arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#     ****************************************
#     *
#     *  routine edits data and error files.
#     *
#     ****************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"


	character*8 iname

	logical errs


#     *** switch back to alpha mode and erase screen ***

	call eralph
	write(ttyout,10)

#     *** if user didn't give file, hard exit ***
	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		return
	}

#     *** tell user which file working with (should be in <dataa>) ***
	write(ttyout,50) idv1,ifl1,ititl
	ifile = ifl1

#     *** see if user wants to continue ***
	write(ttyout,60)
	call crtin
	i = 1
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return

#     *** get error file if any. put in <data> ***
	if (ictrl == ihe) {
		call rederr(idv1,ifl1,ier)
		if (ier != 0) ic = ihx

#        *** check if had i/o error ***
		if (ic == ihx) {
			write(ttyout,70)
			call crtin
			ic = ihx
			return
		}
	}
	ifl1 = ifile

	do  k = 1,maxchn {
		datac(k) = dataa(k)
		error(k) = data(k)
	}
	ic = 0
	write(ttyout,90)

#     *** call appropriate edit routine ***
	while  (ic != ihc)  {
		write(ttyout,100)
		call crtin
		i = 1
		call wjfren(i,x,ic)
		if ((ic == ihx) | ((ic == ihe) & (x == 0.0))) return
		if (ic != ihc) {

#           *** change data and errors if any ***
			if (((ic == ihe) | (ic == 0)) & (i < 80)) {
				call chdata(i,x,ic)
				if (ic == ihx) return

#           *** delete channel ***
			} else if ((ic == ihd) & (x == 0.0)) {
				call chdel(i,x,ic)
				if ((ic == ihe) | (ic == ihx)) return
				ic = 0

#           *** insert channel ***
			} else if ((ic == ihi) & (x == 0.0)) {
				call chinst(i,x,ic)
				if ((ic == ihe) | (ic == ihx)) return

#           *** crt list ***
			} else if ((ic == ihl) & (x == 0.0)) {
				call chlist(i,x,ic)
				if ((ic == ihe) | (ic == ihx)) return

#           *** lp list ***
			} else if ((ic == ihp) & (x == 0.0)) {
				call chprnt(i,x,ic)
				if (( ic == ihe) | (ic == ihx)) return

			} else {
				write(ttyout,200)
				write(ttyout,90)
			}
		}
	}

#     *** read in header information ***
	if (ictrl == ihe) {
		itmp = ifl1
		call filinp(idv1,itmp,errs,ic)
		if ((errs) | (ic == ihx)) {
			write(ttyout,85)
			call crtin
			return
		}
	}

#     *** write history ***
	call namdev(idv1,iname)
	write(ihist,210) iname,ifl1
	return

10      format(' special function f14:'/' routine allows editing of',
	  ' data and'/' the associated standard deviation error',
	  ' files.'//)
20      format(' no spectrum was inputted. press return to',
	   ' hard exit'/)
50      format(' editing applied to ',a,i5,':'/1x,a/)
60      format(' press return to continue or e or x to exit.'/)
70      format(' error in reading errors file.'/' press return',
		' to hard exit.'/)
85      format(' f14 error. fill out specpr bug sheet.',
		' press return to hard exit.'/)
90      format(' edit command format'/4x,'channel  data/e  error',
	  23x,':change'/4x,'d[c]  channel  [channel1  t  channel2]  c',
	  4x,':delete'/4x,'i  channel',35x,':insert after'/4x,
	  'l  channel1  channel2',24x,':list (crt)'/4x,'pd',43x,
	  ':print data (line printer)'/4x,'c',44x,':continue'/)
100     format(' enter edit command'/)
200     format(' illegal command')
210     format(' f14: edited file ',a8,' ',i4,29(' '))
	end
