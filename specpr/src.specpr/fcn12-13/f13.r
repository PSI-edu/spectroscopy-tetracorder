	subroutine f13(ic)
	implicit integer*4 (i-n)
#     ****************************************************
#     *
#     * routine merges 2 files. latest version: 07/05/79
#     *
#     ****************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/f13com"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	character*8 iname

	real*4 dataf(SPMAXCHAN)
	equivalence (dataf,datsc1)

	logical errs

	call eralph
	write(ttyout,10) nchans

#  *** if user didn't give file a, hard exit (should be in <dataa>) ***
	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		return
	}

	ideva = idv1
	ifila = ifl1

50      write(ttyout,60) ideva,ifila,ititl
	call crtin
	i = 1
	call wjfren(i,x,ic)
	if (ic==ihe || ic==ihx) return

#     ********************************************
#     * get second file, file b. put in <datab>.
#     ********************************************

65      errs = .true.
	while  (errs)  {
		write(ttyout,70)
		call crtin
		i = 1
		call wjfren(i,x1,idevb)
		call wjfren(i,xfilb,ic2)

#        *** check for hard or soft exit ***
		if (idevb==ihe || ic2==ihe) {
			ic = ihe
			return
		} else if (idevb==ihx || ic2==ihx) {
			ic = ihx
			return

#        *** check for invalid input ***
		} else if (x1!=0.0 || ic2!=0 ||
		    xfilb<=0.0) {
			write(ttyout,100)

#        *** looks ok so get file b ***
		} else {
			ifilb = xfilb
			itmp = ifilb
			call filinp(idevb,itmp,errs,ic)
			if (ic == ihx) {
				write(ttyout,105)
				call crtin
				return
			}
			if (errs) write(ttyout,115)
		}
	}

	do  j = 1,maxchn {
		datab(j) = data(j)
	}


#     *** read in errors if any ***
	if (ictrl == ihe) {
		errs = .true.
		while  (errs)  {
#           *** read in first error file ***
			itmp = ifila
			call redfil(ideva,itmp,ier)  #position to error record
			ierfl = itmp + 1
			call filinp(ideva,ierfl,errs,ic)
			if (ic==ihx || errs) {
				write(ttyout,105)
				call crtin
				return
			} else {
				do  j = 1,maxchn {
					dataf(j) = data(j)
				}

#              *** read in second error file ***
				itmp = ifilb
				call redfil(idevb,itmp,ier) #position to error record
				ierfl = itmp + 1
				call filinp(idevb,ierfl,errs,ic)
				if (ic == ihx) {
					write(ttyout,105)
					call crtin
					return
				} else if (errs) {
					write(ttyout,140)
					go to 65
				}
			}
		}
	}

	call merger(ic)
	if (ic==ihe || ic==ihx) return

#     *** use header information of file a ***
	call filinp(ideva,ifila,errs,ic)
	if (errs || ic==ihx) {
		write(ttyout,153)
		call crtin
		return
	}

#     *** write history ****

	mhist = ' '


	call namdev(ideva,iname)
	write(ihist,160) iname,ifila

	call namdev(idevb,iname)
	write(mhist(1:74),180) iname,ifilb

	write(mhist(75:148),200) jline

	if (j >= nchans) {
		if (indx == 1) ic = iha
		if (indx == 2) ic = ihb
		write(mhist(149:222),205) ic,ilow
		write(ttyout,190) mhist(149:222)
	}
	return

10      format (15x,' Special Function f13:',//,
		' This routine MERGES TWO data',
		' sets and errors if any.',//,
		' WARNING: merging is LIMITED TO NO MORE than ',
		i6, ' channels',/,
		' (This value, is set by the wavelength set in use.)',
		//)
20      format (' ERROR: NO SPECTRUM WAS INPUT. press return to HARD EXIT.'/)
60      format (' Using for data set "a": ',a,i6,',',//,22x,a,/,
		22x,40('-'),//,
		' Press return to CONTINUE ',
		'or  e  to soft Exit,  x  to hard Exit.'/)
70      format (' ENTER file id and record number for DATA SET "b",',
		' or  e  or  x  to Exit.'/)
100     format (' ERROR: ILLEGAL CHARACTER FOUND. reenter line.'/)
105     format (' PRESS RETURN TO HARD EXIT'/)
115     format (' *** REENTER LINE ***'/)
140     format (' ERROR in reading error record. REENTER',
		' data set b file id and record number.'/)
153     format (' f13 ERROR:  Report specpr bug.',
		' PRESS RETURN TO HARD EXIT.'/)
160     format ('f13:merges 2 files: file a-',a,' r',i5,
		'see manhist')
180     format (' file b-',a,' r',i5,'. merge sequence:')
190     format (a74)
200     format (a74)
205     format (' WARNING: IGNORED MERGING FROM CHANNEL ',a,' ',i6,' on.')
	end
