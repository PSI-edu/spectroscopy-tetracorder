    subroutine listfl (idev)
    implicit integer*4 (i-n)
#ccc
#ccc  version date: 05/20/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc        This subroutine controls the file listing routines.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,whedr,setspool,crtin,wjfren
#ccc  argument list description:
#ccc     arguments: idev
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine controls the file listing routine.         #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

    include "../common/blank"
    include "../common/lbl7"
    include "../common/lbl6"
    include "../common/lbl4"
    include "../common/label1"
    include "../common/labl2"
# Changed to use lbl3a instead of lbl3 to get around case-insensitive
# conflict with ERROR defined in srchdefs.h
    include "../common/lbl3a"
    include "../common/label3"
    include "../common/labelf"
    include "../common/info"
    include "../common/lblvol"
    include "../common/lundefs"
    include "../common/alphabet"
    include "../common/iocontrol"
    include "srchdefs.h"

#RED
    integer*4 redcod     # function redcod
    integer*4 search     # function search
    integer*4 ihchar     # function ihchar

#   icomc: comma control:
#        =1 comma at end of record number, expect more input
#        =0 no comma, end of record input
#
#   iclin = crt line number
    integer*4 filptr, itmpr, icomc, iisav, ihcoma, iclin, err
    integer*4 rbeg, rend
#was:    integer reexec  02/2012 (which is correct?)
    integer*4 reexec
    character restr(1:MAXFIELD)*1
    character*80 namfil, filenm
    integer*2 renum(1:MAXFIELD)
    character strng(1:MAXFIELD)*80
    character*8 inmf


8    iftst=2
    ihcoma = ihchar(',')
    call eralph
    ird=0
    lin=0
    icomc=0
    iauto=0
    iconl=0
    ictrl=0
    iconl=0
    icount=1
    # print option initialization
	mode=0			#defaults laboratory
	mode2=0			#	  no wavelength record printed
	ia=0			#	  title,etc.
	ib=0			#	  no header
	ic=0			#	  no manual history
	icrt=0			#	  crt pause at full page
	id=0			#	  no data
	itf=0			#	  no text
	ih=1			#	  ihist
	lp=0			#	  CRT only
	ii=0			#	  CRT no ihist
	in=0 			#	  CRT full header
	it=0			#	  CRT print text
	iw=0			#	  write file, comma sep. list
	ir=0			#	  read file, comma sep. list
	itw=0			#	  previous record not text

    if (idev==1) {
        iconl= ihl
        go to 70 
    }
    if(idv2==ihl) go to 70 
    ibegn =0
    iea= 0
    ie = 0
    call whedr
        if (idev == 3) {
            write(ttyout,150) inmu
            idv1 = ihu
	    inmf = inmu
        } else if (idev == 4) {
            write(ttyout,150) inmy
            idv1 = ihy
	    inmf = inmy
        } else if (idev == 7) {
            write(ttyout,150) iwrkt
            idv1 = ihd
	    inmf = iwrkt
        } else if (idev == 8) {
            write(ttyout,150) isavt
            idv1 = ihv
	    inmf = isavt
        } else if (idev == 9) {
            write(ttyout,150) iwdgt
            idv1 = ihw
	    inmf = iwdgt
        } else {
		xjunk =0
	}

# mode option laboratory default
1339	i=1
	write(ttyout,89)
	call crtin
while (i<=80) {
	call wjfren(i,x,ie)
	if (ie==ihx | ie==ihe) goto 70
	if (ie==iht) {
		mode=1
	} else if(x!=0) {
            isok=TRUE
            for(i=1;i<=MAXFIELD;i=i+1) {
                restr(i)=' '
                strng(i)=' '
                renum(i)=0
            }
            call recomp(".")

	write (ttyout,170)
	isok=FALSE
	while(isok==FALSE) {
		iclin = 0
		line = 0

1330      i = 1
	iisav = i

1335	i = iisav   # restore saved index and look for next record
        lpline= 0
        ibegn= 0
        call wjfren (i, x, il)
        if (il==ihe  |  il==ihx) goto 70 
	if (i >= 80 & x == 0.0 & icomc == 1) go to 1339  #get next record no.
        if (il!=0 & il != ihcoma) {
		call what(i)
		go to 1339
	}
        ibegn= x
	if (il == ihcoma) {   # comma continuation
		icomc = 1
		iend = ibegn
		iisav = i
		go to 1345
	} else {
		icomc = 0
	}
        call wjfren (i, x, il)
        if (il==ihe  |  il==ihx) goto 70 
        if (il!=0 & il != ihcoma) {
		call what(i)
		go to 1339
	}
        iend= x
	if (il == ihcoma) {
		icomc = 1
		iisav = i
	} else {
		icomc = 0
	}
	if (iend == 0 ) iend = ibegn
1345      if (iend<=0  |  iend+ibegn<=0) {
		call what(i)
		go to 1339
	  }
        if (ibegn<=0) ibegn= 1
        filptr=0
        if((ibegn<=iend) & (ibegn>=1)) filptr= ibegn-1
		isok=TRUE
    }
	is = iha
	go to 40
     }
   }

# crt print options
	write(ttyout,91)
	call crtin
10	i=1
	while(i<=80) {
		call wjfren(i,x,ie)
		if(ie==ihe | ie==ihx ) goto 70
		if (ie==iht) it=1
		if(ie==ihn) in=1
		if (ie==iha) mode2=1
		if(ie==ihh) ii=1
		if(ie==ihw) {           # write a list of comma
			iw = 1		# separated values to a file
		}

		if (ie==ihr) {		# read a list of comma
			ir = 1		# separated values from a file
		}
	}



	if (iw == 1) {			# enter filename for storage
					# of list
		write (ttyout,1)
1		format ('Enter FILENAME to which comma separated ', 
			'list is to be written.')

		call crtin

		i = 0
		call flname (i,iopcon,namfil,err)
		if (err == 1) go to 5
		
		call wcomma(namfil)	# opens file

	}

#
# Enter filename for reading file values option.  Open file, and
# enter range of values to list.
#
		if (ir == 1) {

			write (ttyout,2)
2			format ('Enter FILENAME for READING of list.',
				'  Enter range of values to list.')

			call crtin
			i = 0
			call flname (i,iopcon,filenm,err)
			if (err == 1) go to 5

			open (redlun, file=filenm, iostat=ier,
				status='unknown', form='formatted', access=                                     'sequential')
			rewind redlun
		

		# find range for list

			do k=i,80 {           	
				if (iopcon(k:k) != ' ')  {
					go to 12
				}else{
					next
				}
			}

			if (k == 81) {
				write (ttyout,14)
14				format ('Enter range of values to list')
				call crtin
				i=1
			}
				
		# beginning value rbeg

12			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				write (ttyout,13)
				go to 5
			}
			rbeg = x

		# ending value rend

			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				write (ttyout,13)
13				format ('Error, STRANGE character ',
					'encountered in string.')
				go to 5
			}
			rend = x
		}
    
5   write(ttyout,90)
    call crtin
7   i=1
	while (i<=80) {
		call wjfren(i,x,ie)
		if (ie==ihx  |  ie==ihe) goto 70
# line print options
		if (ie==ihh) ih=0
		if (ie==iht) itf=1
		if (ie==ihm) im=1
		if (ie==ihd) {
			id=1
			ic=1
			ib=1
			ia=0
			lp=1
		}
		if (ie==ihc){
			ic=1
			ib=1
			lp=1
		}
		if (ie==ihb){
			ib=1
			ia=0
			lp=1
		}
	
		if (ie==iha){ 
			ia=1
			lp=1
			icrt=1

		}

		if(ie==ihp)  {
			ia=1
			lp=1
		}

}


	if (lp==1) {
		call setspo
		lp=lp+1
	}
#
#  string search
#
15  isok = FALSE
    while(isok == FALSE) {
	write (ttyout,160)  #prompt to enter search string
        call crtin
        if (iopcon != ' ') {
            isok = redcod(renum,restr,strng)
            if (isok == EXIT) {
                call dumpsp
                idev=ihx
		rferfl(1) = 1
                return
            }
        } else {
            isok=TRUE
            for(i=1;i<=MAXFIELD;i=i+1) {
                restr(i)=' '
                strng(i)=' '
                renum(i)=0
            }
            call recomp(".")
        }
    }
1114    write (ttyout,170)
	isok=FALSE
	while(isok==FALSE) {
		iclin = 0
#
# read file of comma separated list  option
#
25	if (ir == 1) {
		read (redlun,26,end=65) iopcon
26		format (a)
	}else{
		write(ttyout,100)
		line = 0
		call crtin
	}
#
# decode beginning and ending files to be listed
#


30      i = 1
	iisav = i

35	i = iisav   # restore saved index and look for next record
        lpline= 0
        ibegn= 0
        call wjfren (i, x, il)
        if (il==ihe  |  il==ihx) goto 70 
	if (i >= 80 & x == 0.0 & icomc == 1) go to 25  #get next record no.
        if (il!=0 & il != ihcoma) next 
        ibegn= x
	if (il == ihcoma) {   # comma continuation
		icomc = 1
		iend = ibegn
		iisav = i
		go to 45
	} else {
		icomc = 0
	}
        call wjfren (i, x, il)
        if (il==ihe  |  il==ihx) goto 70 
        if (il!=0 & il != ihcoma) next
        iend= x
	if (il == ihcoma) {
		icomc = 1
		iisav = i
	} else {
		icomc = 0
	}
	if (iend == 0 ) iend = ibegn
45      if (iend<=0  |  iend+ibegn<=0) next
        if (ibegn<=0) ibegn= 1
        filptr=0
        if((ibegn<=iend) & (ibegn>=1)) filptr= ibegn-1
		isok=TRUE
    }
	is = iha
40	if (iclin == 0) {
		 call eralph
	    if (mode2 != 1) {
		 if (mode==0) {
			 write(ttyout,120) 
		}else{
			write(ttyout,121)
		}
	    }else{
		write (ttyout,122)
	    }
	}
#
# Check to determine if value is in specified range for reading file
# option
#
	if (ir == 1) {
		if (ibegn < rbeg | iend < rbeg ) {
			iclin = 1
			go to 25
		}else if (ibegn > rend | iend > rend) {
			ir = 0
			write (ttyout,41)
41			format ('Reached end of range of read listing ',
				'file')
			rewind redlun
			go to 60
		}else if (icount == 1) {
			iclin = 0
		}
	}

	if (ir == 2) {
		ir = 0
		write (ttyout,42)
42		format ('Reached end of read listing file')
		go to 60
	}

# 
#     print 17 files on crt at one time 
# 
2000	if (iclin <= 17){
2100	rferfl(1) = 0		# do not issue continuation error msg
	iclin=iclin+1
	if (ii==1) {
		iclin=iclin+1
	}
        if (filptr >= iend & is != ihc) {
		if (icomc == 0) go to 60
		if (icomc == 1) go to 35
	}
#		print *,"filptr = ",filptr," iclin = ",iclin," iend = ",iend
        filptr = filptr + 1
        if (filptr > iend) iea=0
        call devsta (idev,ista,1,iprt)
        if (iprt!=-1) {
		 if (filptr > iabs(iprt)) go to 60
	}

# make temporary file pointer before read, because filptr gets
#	incremented on multi-segment files.

	itmpr = filptr
        call redfil (filptr, idev, iftst)

# check to see if this was a continuation record

	if ((iftst != 0) & (rferfl(2) == 1)) {
		iclin = iclin-1
		go to 2000
	}

        if (iftst!=0) {
            if (iftst==32) {
                if (ie==ihp) write(lstlun,130)
                line= line+ 2
            }
	    go to 60
        }

        itest=search(renum,restr,strng)
        if (itest == 0) {
		if ((ir == 1) & (icount < 17)) {
			read (redlun,51,end=65) iopcon
51			format (a)
			icount = icount + 1
			go to 30
		}else{
			iclin=iclin-1
			go to 2000
		}
	}

        airmas= irmas
        airmas= airmas/ 1000.
        if (airmas > 20.0) airmas= 20.0


#
#     write to crt display
#
	irec = itmpr
	call cprint(mode,mode2,ii,in,it,irec,iclin,itmpr)
	if (lp > 1) {
		call lprint(mode,ia,ib,ic,id,itf,ih,im,irec,lpline,itw,inmf)
	}

#
# Write comma separated list to file option
#

	if (iw == 1) {			# write to a file, a comma
					# separated list
			write (wrtlun,55) itmpr
55			format (i6,',')
	}

#
#     if ie= p then print on line printer and calculate line number on
#     page (=line)
#
        if ((ie==ihp) & (ihist(1:2)!='  ')) line= line+ 2
        if (lin==2) lin= 0
        if (line>=50) line= 1
#timj was ==
#       if (filptr >= iend) {
#		write (ttyout,140)
#		go to 60
#	}
    }
#
# read comma separated list from file option
#
	if ((ir == 1) & (icount < 17)) {
		read (redlun,61,end=65) iopcon
61		format (a)
		icount = icount + 1
		go to 30
	}else{
		icount = 1
		go to 62
	}


65	ir = 2
 	iclin = 0
	rewind redlun
	close (redlun,iostat=ier)
	go to 60


62	if (icrt == 1) go to 2100
   	if (iclin < 17) go to 2000         #fake do loop with                                                              #modification
               	                           # of do index. end of loop
 	call recomp(".")
	if (iea==iha) call eralph
	if (iea==iha) {
		iclin = 0
		go to 40
    	}

60      write(ttyout,110)
	iclin = 0
        call crtin



    i=1
    call wjfren (i,x,is)

	if (is!=ihc) {
		ir = 0
		iw = 0
		close (redlun,iostat=ier)
    		close (wrtlun,iostat=ier)
	}

	if (is==ihr) go to 8
	if (is == ihs) goto 15
	if (is != iht & is != ihm) {
	        if (is==ihx  |  is==ihe) goto 70 
       		if(is==ihc) {
			iclin = 0
			go to 40
		}

	        call wjfren (i,x2,il)
       	 	if ((il==0 | il==ihcoma)& (is==0 | is==ihcoma)& 
			(x>=0.)& (x2>=x | is == ihcoma)& 
			(x2!=0)) goto 30 
        	if (il==ihn) go to 10 
        	go to 60
	}
    if (lp > 1) {
	call dumpsp
    }
    idev=is
    rferfl(1) = 1
    return
70  continue
    if (lp > 1) {
	call dumpsp
    }
    idev = ihx
    rferfl(1) = 1
    return
80  format (1h , i4, 1x, a, i5, 1x, 2(a,':'), a, 1x,
       2(a,'/'), a, f6.3)
89 format(3x,'type  t  for TELESCOPIC or  l  for LABORATORY (DEFAULT)'/,
	     '  or beginning and ending files to be listed')
90 format( 'LINEPRINTER OPTIONS (return for CRT only)',/,3x,'p  print ', 
	'titles + selected fields given by print mode',/,3x,
	'a  print titles with no pausing at CRT page full',//,3x,
	'with p or a, also select:',/3x,'h   no history to be printed ',/,
	3x,'m  manual history if not blank',/,3x,'t  print text on text',
	' data files',/,3x,'b  print all header info except manual history',
	/,3x,'c  print all except data',/,3x,'d  print all and data',/)
91	format(3x,'*** CRT print options ***',/,'n  print # only',
	' on CRT when printing to lineprinter',/,'a  print wavelength',
	' record numbers',/,
	'h  print history ',/,'t  print text',
	/,'r  read and print a list of comma separated record ',
	'numbers',/,'w  write to a file, a list of comma separated',
        ' record numbers' ,/, '   You will be prompted for the file name',
        ' after you select r or w.')
100 format (3x, 'Type in the beginning and ending files to be listed',/)
110 format (1x,78(1h-),/,' Type  c  to continue, e or x  to exit, ', 
         ' Type in new record numbers to list',/,' type  r  to return',
         ' to options, type t to go to file display and transfer',/, 
	'or type m to go to math routines') 
120 format (1x,'file',12x,'title',28x,'chans',4x,'time',8x,'date')
121 format(1x,'file',12x,'title',28x,'chans',4x,'date',7x,'airmass')
122 format(1x,'file',12x,'title',28x,'chans',4x,'date',8x,'waves')
130 format (  10x,' EOF read from tape',/, 1x, 130(1h-))
140 format (1x, 78(1h-))
150 format(27x,'LISTING OF FILE: ',a,/)
160 format (1x,
          /, 'Enter SEARCH STRING: letter "string1" letter "string2" etc.',
          /, '       where letter is for t  title',
          /, '                           h  history',
          /, '                           d  date',
          /, '                           m  manual history',
	  /, '       maximum number of strings is 4',
          /, '       comparisons is and (default), use | for or, & ',
						'for and')
170 format (/)
180 format (1x,i5, ' continuation record')
    end
