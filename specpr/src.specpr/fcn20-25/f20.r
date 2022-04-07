    subroutine f20(ic)
    implicit integer*4 (i-n)
#ccc  version date: 06/01/83
#ccc  author(s): Rodney Kam
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine reads data from a user inputted
#ccc         file and writes it to a specpr file . or reads a
#ccc         specpr file and writes it to a file in 1pe or f
#ccc         file.
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          rstsrt,hreset,crtin,wjfren,devlun,redfil,devsta,rederr
#ccc  argument list description:
#ccc                         none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:

	include "../common/spmaxes"   # max parameters, must be first

    include "../common/blank"
    include "../common/lbl6"
    include "../common/lbl3"
    include "../common/label1"
    include "../common/lbl7"
    include "../common/lblg"
    include "../common/lbl4"
    include "../common/lbl8"
    include "../common/alphabet"
    include "../common/lundefs"

#RED
    integer*4 lnb     # function lnb
    integer*4 mmm, ddd, yyy

	integer*4 hr,minute
	real*4          sec

    character*47 inputf
    character*1 icrrt

    integer*4 ier, ihmul
    real*4 xmul

#
    icrrt = char(13)     # carriage return character
    ihmul = ihchar('*')  # asterisk fr multiplication
    xmul = 1.0           # multiplier

    call eralph
1   write(ttyout,10)

    write(ttyout,4)nchans
2   itil = 0
    ihst = 0
    imanl = 0
    iman= 0
    ic =0
    write(ttyout,11)
#.........
# decode command to write or read
#.........
    call crtin
    nchar = 1
    call wjfren(nchar,x,il)
    if (il == 0) goto 2
    if (il == ihr) irw = ihr
    if (il == ihx) goto 9999
    if (il == ihw) irw = ihw

    if (irw!=ihw && irw!=ihr) goto 2
    for(;;) {
        call wjfren(nchar,x,il)
        if (nchar >= 80) break
        if (il == 0) {
            write(ttyout,12)
            goto 2
        }
        if (il == iht) itil = 1
        if (il == ihh) ihst = 1
        if (il == ihe) ictrl = ihe
        if (il == ihm) {
            iman = 1
            call wjfren(nchar,x,il)
            if (il != 0) {
                write(ttyout,12)
                goto 2
            }
            if (nchar >= 80) {
                write(ttyout,12)
                goto 2
            }
            imanl = x
            if (imanl<1 || imanl>4) {
                write(ttyout,13)
                goto 2
            }
        }
	if (il == ihmul) {
		call wjfren(nchar,x,il)
		if (x > 0.1e-30) {
			xmul = x
		}
	}
    }
    if (irw == ihw){ 
        ic=ihx
        write(ttyout,5)
5       format (' type:',/,
                '        f: for WRITING data in f format,'/,
                '        e: for WRITING data in 1pe15.6 format',/)
        call crtin
        nchar=1
        call wjfren(nchar,x,it)
    }
#..........
# get the file that is to be read or written to
#..........
6   write(ttyout,20)
    call crtin
    nchar=1
    call getstr(nchar,inputf,ierr)
    if (inputf == ' ') goto 6
    if (inputf(1:2)=='e ') {
		ic = ihe
		goto 9999 
    }
    if (inputf(1:2)=='x ') {
		ic = ihx
		goto 9999 
    }
    close(1,iostat=ier) 
    if (irw == ihw) {   # open new file for writing
          open (1,file=inputf,status='NEW',iostat=ier)
    } else {            # open existing file for reading
          open (1,file=inputf,status='OLD',iostat=ier)
    }
    if (ier != 0) {
		write (ttyout,*) 'OPEN ERROR', ier
		if (irw == ihw && ier == 17) {
			write (ttyout,*) 'gfortran error 17 means file exists, but it should not for writing a file'
		}
		if (irw == ihr && ier == 2) {
			write (ttyout,*) 'gfortran error 2 means file does not exist, but it should for reading a data'
                }
		write (ttyout,*) 'Press return to renter, e or x to exit'
		call crtin
		if (il == ihx) {
			ic = ihx
			goto 9999
		}
		if (il == ihe) {
			ic = ihe
			goto 9999
		}
		go to 6
    }
    rewind 1
#................
#  write data from input file to datac
#................
    if (irw == ihw) {
7       call whedr2
        write(ttyout,971)
971     format(1x,'Do you want to INCLUDE WAVLENGTHS?',/,
                  '   (Type y for yes, n for no.)',/)
        call crtin
        nchar=1
        call wjfren(nchar,x,ig)
        if(ig==ihy) {
            irg=ihy
            call wavlng(itrol(1),itrol(2),ier)
        }
        else
            if(ig==ihn)
                irg=ihn
            else 
                goto 7
3       write(ttyout,75)
#.............
#  decode file id
#.............
        call crtin
        nchar = 1
        call wjfren(nchar,x,id)
        if (id == 0) goto 3
        if (id == ihe) {
		ic = ihe
		goto 9999
	}
        if (id == ihx) {
		ic = ihx
		goto 9999
	}
        call devlun(4,id,lun)
        call devsta(lun,ista,0,iprt)
        if (ista<=0 || lun==0) {
            write(ttyout,61)
            goto 3
        }
#.............
#  decode file number
#.............
        call wjfren(nchar,x,is)
        if (is != 0) goto 3
        if (is == ihe) {
		ic = ihe
		goto 9999
	}
        if (is == ihx) {
		ic = ihx
		goto 9999
	}
        ifil = x
	itmp = ifil
        call redfil(itmp,lun,iflg)
        if (iflg != 0) {
		ic = ihe
		goto 9999
	}
        if (itil != 0) write(1,310) ititl
        if (ihst != 0) write(1,310) ihist
        if (iman != 0) {
            for(i=1;i<=imanl;i=i+1) {
                is = (i-1)*74 + 1
                ie = is + 73
                write(1,310) mhist(is:ie)
            }
        }
#timj 256=>nchans
        for(i=1;i<=nchans;i=i+1) datac(i) = data(i)
        if (ictrl == ihe) {
            call rederr(ifil,lun,iflg)
            if(iflg != 0) {
		ic = ihe
		goto 9999
		}
        }
        for(i=1;i<=nchans;i=i+1) {
            if (ictrl == ihe) {
                if(irg==ihn){
                    if(it==ihe){
                        write(1,300) datac(i),data(i)
                    }
                    if(it==ihf){
                        write(1,301) datac(i),data(i)
                    }
                }
                if(irg==ihy){
					#write waves, data, errors
                    if(it==ihe){
                        write(1,300) dataa(i),datac(i),data(i)
                    }
                    if(it==ihf){
                        write(1,301) dataa(i),datac(i),data(i)
                    }
                }
            } else {
                if(irg==ihn){
                    if(it==ihe){
                        write(1,300) datac(i) #write data
                    }
                    if(it==ihf){
                        write(1,301) datac(i) #write data
                    }
                }
                if(irg==ihy){
                    if(it==ihe){
                        write(1,300) dataa(i),datac(i) #write waves, data
                    }
                    if(it==ihf){
                        write(1,301) dataa(i),datac(i) #write waves, data
                    }
                }
            }
        }
	call eralph
	write (ttyout,"('OPERATION COMPLETE, going to beginning:',//)")
	go to 1         # complete, begin again again
    } else {
#
#
#  Read data from input file
92      write(ttyout,99)
        call crtin
        ich = 1
        call wjfren(ich,x,il)
        if (il==ihe || il==ihx) {
            ic = il
            goto 9999
        }
        if (il != 0) {
		call what(ich)
		goto 92
	}
        ncol = x
        if (ncol<1 || ncol>79) ncol = 1
        call wjfren(ich,x,il)
        if (il != 0) goto 92
        if (ich >= 80)  {
            nstrow = 1
            nenrow = maxchn
        } else  {
            nstrow = x
            call wjfren(ich,x,il)
            if (il != 0 ) go to 92
            if (ich >= 80) nenrow = nstrow + maxchn -1
            else nenrow = x
        }
        if (nstrow < 1) nstrow = 1
        if (nenrow-nstrow<0 || nenrow-nstrow>maxchn) {
            write(ttyout,81)
            goto 92
        }
#
# ask user how often to write the data that is read to the CRT
#
93      write (ttyout,100)
	call crtin
	ich = 1
	call wjfren(ich,x,il)
        if (il==ihe || il==ihx) {
            ic = il
            goto 9999
        }
        if (il != 0) {
		call what(ich)
		goto 93
	}
	iline = x
	if (iline < 1) iline = 1
	write (ttyout, "(' Printing every',i7,' th line.')") iline

# initialize the header info

	istb = 0
	revs = 1
	irmas  = 0
	nruns = 1
	siangl = 1
	seangl = 1
	sphase = 1
	iwtrns = 1
	itimch = 1
	xnrm = 1.0
	scatim = 1.0
	timint = 1.0
	iwtrns = 1
	iband(1) = 1
	iband(2) = 1
	timint = 1.0
	xnrm   = 1.0
	tempd  = 273.0
	usernm = '        '
        ira(1) = 0
        ira(2) = 0
        ira(3) = 0
        idec(1) = 0
        idec(2) = 0
        idec(3) = 0

	# set the date to the current system  date and UT time
	call jdatim(jdatea,iscta)
	jdateb = jdatea
	isctb=iscta
	#write (ttyout,*) "DEBUG: jdateb, isctb =", jdateb, isctb

	call getime (hr,minute,sec,ier)
	isgn=1
	call frdms (iscta,24000,hr,minute,sec,isgn)
	isctb = iscta

	for (i=1; i<= 296; i=i+1) {
		mhist(i:i) = ' '
	}

	revs1 = 1.0
	revs2 = 1.0
	xnrma = 1.0
	xnrmb = 1.0
	sctma = 1.0
	tminta = 1.0
	tmintb = 1.0

	iraa(1) =0
	iraa(2) =0
	iraa(3) =0
	irab(1) =0
	irab(2) =0
	irab(3) =0
	ideca(1) =0
	ideca(2) =0
	ideca(3) =0
	idecb(1) =0
	idecb(2) =0
	idecb(3) =0
	irmasa =0
	irmasb =0
	itmcha =1
	nruna =1
	iwtrna =1
	iwtrnb =1
	ifutx =1

	mhista= mhist
	mhistb= mhist

	mdatea1 = jdateb
	mdatea2 = jdateb
	mdateb1 = jdateb
	mdateb2 = jdateb

	mcta1 = isctb
	mcta2 = isctb
	mctb1 = isctb
	mctb2 = isctb
	mstb1 = isctb
	mstb2 = isctb

	
#...............
# read in title and histories
#...............
        if (itil != 0) {
            read(1,91,end=1000) iopcon
            ititl1 = iopcon
        }
        if (ihst != 0) {
            read(1,91,end=1000) iopcon
            ihist = iopcon
        }
        if (iman != 0) {
            iis=1
            iie = 37
            for(i=1;i<=imanl;i=i+1) {
                read(1,91,end=1000) iopcon
                  if (i == 1) {
                    mhist(1:74) = iopcon
                  } else if (i == 2) {
                    mhist(75:148) = iopcon
                  } else if (i == 3) {
                    mhist(149:222) = iopcon
                  } else if (i == 4) {
                    mhist(223:296) = iopcon
                  }
            }
		mhista= mhist
		mhistb= mhist
        }

#...............
#  read numbers in file
#...............
        for(i=1; i<=nstrow-1; i=i+1) read(1,91,end=1000) iopcon
        for(i=1; i<=(nenrow-nstrow)+1; i=i+1) {
            read(1,91,end=1000)iopcon
91          format(a)
#
# Modification to catch tabs
# Added 08/24/1987      NSG
#
		do incr=1,80 {
			if (iopcon(incr:incr)==char(9)) {
				call what(incr)
				write (ttyout,9998)
9998	format ('***** ERROR *****   Data CANNOT CONTAIN TABS...quitting',
		'PRESS RETURN to exit')
				call crtin
				ic=ihx
				close(1,iostat=ier)
				return
			}
		}
            nchar=ncol
            call rlchng(nchar,x,ierr)
            if (mod(i,iline) == 0) {
                if (ictrl != ihe) write(ttyout,94)icrrt,i,x
            }
            if (ierr.ne.0) {
                write(ttyout,60)
		write(ttyout,91) iopcon
		call what(nchar)
		call crtin
		ic = ihe
                goto 9999
            }
            datac(i) = x * xmul
            data(i) = x * xmul
#...............
# errors if any
#...............
            if (ictrl == ihe) {
                call rlchng(nchar,x,ierr)
                if(ierr.ne.0) {
                    write(ttyout,60)
			write(ttyout,91) iopcon
			call what(nchar)
			call crtin
                    ic = ihe
                    goto 9999
                }
                error(i) = x
		if (mod(i,iline) == 0) {
	                write(ttyout,123) icrrt,i,data(i),error(i)
		}
            }
        }
    }
1000 continue

    if (irw == ihw){ 
        write(ttyout,97)
    } else {
        write (ttyout,98)
    }
    call crtin
    nchar=1
    call wjfren(nchar,x,il)
    if (il == ihr) goto 1
    if (il == ihe) {
		ic = ihe
		go to 9999
    }
    if (il == ihx) {
		ic = ihe
		go to 9999
    }
    else if (il != 0) goto 1000
#................
#  history
#................
    if (ihst == 0) {
	itmp = lnb(inputf)
	itmp1 = 1
	if (itmp > 40) itmp1 = itmp-39
        if (ictrl == ihe) write(ihist,70) inputf(itmp1:itmp)
        else {
            write(ihist,71) inputf(itmp1:itmp)
        }
    }

    goto 9999

4   format(' Number of Channels = ',i7)

10  format(///,
' Function 20:',/,
'      This function READS (or WRITES) data from (or to) a file',/,
' data can be in f or e format in the read mode and writes',/,
' in the 1pe format.  Errors can be included for both reads and',/,
' writes.',/ )

11  format('Type:',/,
'       r  for READING -or-',/,
'       w  for WRITING',/,
' Options:',/,
'       e  include ERRORS',/,
'       t  include TITLE',/,
'       h  include HISTORY',/,
'       m# include # lines of MANUAL HISTORY',/,
'       *x.x   multiplier (default = *1.0)' )

12  format(' **INCORRECT ENTRY of options... Reenter')

13  format(' **INCORRECT NUMBER OF LINES of manual history')

20  format(//,' Enter input/output FILENAME. (47 characters or less)',
           /,' ---------------------------------------------|',/)

60  format(//,' ILLEGAL DATA type...')

61  format(' INCORRECT ENTRY retype!')

70  format('f20:from ',a,' w/er')

71  format('f20:from ',a,'     ')

75  format(' Type FILE ID and record number of specpr data file.',/)

81	format('....INCORRECT RANGE of rows')

%94	format(a,' data point ',i7,'   data = ',1pe15.6,'     ',$)

97	format(/,' Type  r  to RETURN to beginning of f20',/,
                 '          (to write another file or read one)', /,
	     ' Press  return  to EXIT f20.',/)

98	format(/,' Type  r  to REDO  -or-  ', /,
	     ' Press  return  to EXIT f20 and WRITE RESULTS',/)

99	format(//,' Enter STARTING COLUMN, STARTING ROW, and ENDING ROW ',/)

100	format (/,' Enter how often to write data that is read from file',/,
		' to the CRT (default = every line).',/,
		'             Example: 10 = every 10th line.')
# RED Added SOLARIS and SUNOS 
#NONHPUX%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#NONHPUX%     c e15.6,$)
#SOLARIS%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#SOLARIS%     c e15.6,$)
#SUNOS%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#SUNOS%     c e15.6,$)
#DEC%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#DEC%     c e15.6,$)
#HPUX%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#HPUX%     c e15.6,$)
#LINUX%123    format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',e15.6,$)
#IA64HPUX%123	format(a,' Data point ',i7,'  Data = ',e15.6,'  Error = ',
#IA64HPUX%     e15.6,$)

300 format(3(1pe15.6))

301 format(3f15.6)

310 format(a)
9999 continue
    close(1,iostat=ier)
    return
    end
