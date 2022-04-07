        subroutine f40(ic)
	implicit integer*4 (i-n)
#     ****************************************************
#     *
#     * routine to calculate standard deviations between 2 files
#     * modified from subroutine f13 08-22-86, f.kruse


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
        real*4 stdev(SPMAXCHAN)

	equivalence (datac,stdev) 

        
	logical errs

	call hreset(1)
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

#     *** calculate deviation of file b from file a ***

         c=.025
       do i  = 1,maxchn {
                stdev(i)=((1-((dataa(i)+c)/(datab(i)+c)))**2)/2
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

10      format (' special function f40: calculate std. deviation',
                ' between two spectra'/,
                ' if no. channels is > ',i3,
                ' (last wavelength file)',
                ' the extra channels are ignored'/)
20      format (' no spectrum was input. press return to hard exit.'/)
60      format (' using for file a: ',a,i5,','/1x,a,/,
		' press return to continue or e to soft exit, x to hard exit.'/)
70      format (' enter id and number for second file, file b,',
		' or e or x to exit.'/)
100     format (' illegal character found. reenter line.'/)
105     format (' press return to hard exit'/)
115     format (' *** reenter line ***'/)
140     format (' error in reading error file. reenter',
		' file b.'/)
153     format (' f40 error. fill out specpr bug sheet.',
		' press return to hard exit.'/)
160     format ('f40: std dev. for 2 files: file a-',a8,' f',i5,
		'see manhist')
180     format (' file b-',a8,' f',i5,'. std dev calculation:')
190     format (a74)
200     format (a74)
205     format (' ignored stdev calculation from channel ',a,' ',i5,' on.')
	end
