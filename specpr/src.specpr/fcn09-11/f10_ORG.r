	subroutine f10(ic)
#
# sort into increasing wavelengths
#
	implicit integer*4(i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
 	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lbl6"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	dimension 	isort(SPMAXCHAN)
	equivalence (isort,datsc3)
	character*8	inm

	logical errors, madeit, nochng
#RED
	integer*4 iwidok      # function

	data ihplus/'+'/


	if (ictrl == -1) {
		ic = ihx
		return
	}
	do i=1,maxchn
		data(i)=dataa(i)
#------------------------------------------------
#  print info
#--------------------------------------------------
	nochng = .false.
	repeat {
		call hreset(1)
		call whedr2
		write(ttyout,10)
		write(ttyout,20) idv1,ifl1,ititl
		if (ictrl == ihe) {
			errors = .true.
			write(ttyout,30)
		} else {
			errors = .false.
			write(ttyout,35)
		}
#-----------------------------------------------
#  new wavelength file necessary ?
#-----------------------------------------------
		madeit = .false.
		repeat {
			write(ttyout,37)
			call crtin
			i = 1
			call wjfren(i,x,il)
			if (iwidok(il) == 1) {
				iwtmpf = il
				call wjfren(i,x,il)
        			if (i<80 && il==0) {
        				iwrec=x
        				call wavlng (iwtmpf,iwrec,ier)
        				if (ier==0) madeit=.true.
        			}
			} else if (il==ihx || il==ihe) {
				ic = il
				return
			} else if (x == 0.  &  il == 0) {
				madeit = .true.
				nochng = .true.
			}
		} until (madeit)
	} until (nochng)
#----------------------------------------------------
#  here data array has data and dataa has wavelengths
#
#  now do the sorting of data (not wavelengths)
#----------------------------------------------------
	call bubble(dataa, isort, nchans)

	do  i = 1, nchans
		datac(i) = data(isort(i))
#------------------------------------------------------
#  do errors if required
#------------------------------------------------------
	if (errors) {
		idad = 2
		itmp = ifl1
		call devlun(4,idv1,lun)
		if (lun == 0) {
			write(ttyout,40)
			call crtin
			ic = ihx
			return
		}
		call finfil(itmp,lun,2,ier)     # position before errors
		itmp = itmp +1                  # compute error record number
		ifl1e = itmp
		call finfil(itmp,lun,2,ier)     # get errors
		if (ier != 0) {
			write(ttyout,45) ier,ifl1e
			call crtin
			ic = ihx
			return
		}
		do  i = 1, nchans
			error(i) = datab(isort(i))
	}
#---------------------------------------------------
#  dataa has unsorted wavelengths
#  datac has data (sorted)
#  errors has sorted errors
#------------------------------------------------------
#----------------------------------------------------
#  re-read data into dataa
#----------------------------------------------------
	call devlun(4,idv1,lun)
	itmp = ifl1
	call finfil(itmp,lun,1,ier)

	if (ier != 0) {
		write(ttyout,45) ier,ifl1
		call crtin
		ic = ihx
		return
	}
	ic = 0
#------------------------------------------------------
#  update the history
#------------------------------------------------------
	call namdev(idv1,inm)
	write(ihist,60) inm,ifl1
	mhist(1:274) = ' '
	return
#-----------------------------------------------------
10    format('Function f10: Sort data into Increasing Wavelength Order'//)
20    format(5x,'This function sorts data into increasing wavelength',/,5x,
	' order with the errors if included.',//,5x,
	'Operating on:',a,i5,':',a,//)
30    format(' Sorting Errors Also'/)
35    format(' Errors Not Included'/)
37    format(' Type  e  to EXIT from f10'/,
	   ' Type  x  for HARD EXIT (no more processing)'/,
	   ' Type record id (V, W, D, U, or Y and record number to',
		' CHANGE Wavelength Set'/,
	   ' Type return to continue'/)
40    format(' LUN ERROR!  Press return to EXIT'/)
45    format(' I/O ERROR ',i5,' in file ',i4/)
60    format(' f10: ',a8,' record: ',i5,' sorted to incr. wavelengths')
#--------------------------------------------------------------------

	end
