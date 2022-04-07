subroutine getopt

#************************************************************************
# This subroutine finds the command line arguments and passes the 
# appropriate variables. 
#
#
	implicit integer*4 (i-n)

	include "../../src.specpr/common/lbl4"
	include "spfeat.h"
	include "cmdarg.h"

#######
# NOTE: the following line contains executable code so it must be
# the last include with definitions
#######
	character*80 deffile, bdanalfile
        include "defaultfile.h"

#
	ttyin=5
	ttyout =6
#
#
	n =ncmdarg
#  
# set defaults
		widmin=0.0
		widmax=1.0e30
                errmin=0.0
                errmax=1.0e30
                asymin=0.0
		asymax=1.0e30
		depmin=0.0
		depmax=1.0e30
		cenmin=0.0
		cenmax=1.0e30
                wflag=0
                eflag=0
		dflag=0
		cflag=0
		aflag=0
		oflag=0
                ferror=0
                head=0
		verb=0
		band=0
		filnam= deffile


#
#
# read commands
#
	i=0
100 	i=i+1
	if (i.gt.n)  { 
		return 
	}

	strbuf = charg(i)

	do j = 2, 80 {
		if (strbuf(j:j)== ' ' & strbuf(j:j)== char(0)){
			break
		}

# option -F filename input
#
	if (strbuf(j:j) .eq. 'F') {
		if (strbuf(j+1:j+1) .ne. ' '& 
		    strbuf(j+1:j+1) .ne. char(0)) {
			write(ttyout,1000)
1000			format('no blank after  -F option')
			go to 2000
		}            
	i=i+1  
	strbuf = charg(i)
	filnam = strbuf
	go to 100
   	}

# option -o output filename: for output to another binary file
#
	if (strbuf(j:j) .eq. 'o') {
		if (strbuf(j+1:j+1) .ne. ' '& 
		    strbuf(j+1:j+1) .ne. char(0)) {
			write(ttyout,1700)
1700			format('no blank after  -o option')
			go to 2000
		}            
	i=i+1  
	strbuf = charg(i)
	ofile = strbuf
	oflag=1
	go to 100
   	}

#
# option -w width
#
		if(strbuf(j:j) .eq. 'w') {
                        wflag=1
# check j+1 is blank or null
			if (strbuf(j+1:j+1) .ne. ' ' & 
			    (strbuf(j+1:j+1) .ne. char(0))){
				write(ttyout,1010)
1010     			format('No blank after -w option')
			go to 2000
			}

		i=i+1
		strbuf = charg(i)
		iopcon=strbuf
		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
		k= 1
		call wjfren (k,x,il)

		if (il.ne.0){
			write(ttyout,1020)il,iopcon
1020			format('strange character ', 
				a,'encountered in line: ',a)
 				 go to 2000                  
		}
		widmin = x

		i=i+1
1025		strbuf = charg(i)
		iopcon=strbuf
# check for nulls

		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
  
		k = 1
		call wjfren(k,x,il)

		if (il .ne. 0) {
			write(ttyout,1020)il,iopcon 
		go to 2000	
		}

		widmax = x

		if (widmin .ge. widmax) { 
			write(ttyout,1030)
1030			format('width min > width max ')
		go to 2000	
		}
	}




# option -d depth
#
		if(strbuf(j:j) .eq. 'd') {
                        dflag=1
# check j+1 is blank or null
			if (strbuf(j+1:j+1) .ne. ' ' & 
			    (strbuf(j+1:j+1) .ne. char(0))){
				write(ttyout,2010)
2010     			format('No blank after -d option')
			go to 2000
			}

		i=i+1
		strbuf = charg(i)
		iopcon=strbuf
		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
		k= 1
		call wjfren (k,x,il)

		if (il.ne.0){
			write(ttyout,2020)il,iopcon
2020			format('strange character ', 
				a,'encountered in line: ',a)
 				 go to 2000                  
		}
		depmin = x

		i=i+1
2025		strbuf = charg(i)
		iopcon=strbuf
# check for nulls

		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
  
		k = 1
		call wjfren(k,x,il)

		if (il .ne. 0) {
			write(ttyout,2020)il,iopcon 
		go to 2000	
		}

		depmax = x

		if (depmin .ge. depmax) { 
			write(ttyout,2030)
2030			format('depth min > depth max ')
		go to 2000	
		}
	}




# option -c center
#
		if(strbuf(j:j) .eq. 'c') {
                        cflag=1
# check j+1 is blank or null
			if (strbuf(j+1:j+1) .ne. ' ' & 
			    (strbuf(j+1:j+1) .ne. char(0))){
				write(ttyout,2110)
2110     			format('No blank after -c option')
			go to 2000
			}

		i=i+1
		strbuf = charg(i)
		iopcon=strbuf
		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
		k= 1
		call wjfren (k,x,il)

		if (il.ne.0){
			write(ttyout,2120)il,iopcon
2120			format('strange character ', 
				a,'encountered in line: ',a)
 				 go to 2000                  
		}
		cenmin = x

		i=i+1
		strbuf = charg(i)
		iopcon=strbuf
# check for nulls

		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
  
		k = 1
		call wjfren(k,x,il)

		if (il .ne. 0) {
			write(ttyout,2120)il,iopcon 
		go to 2000	
		}

		cenmax = x

		if (cenmin .ge. cenmax) { 
			write(ttyout,2130)
2130			format('center min > center max ')
		go to 2000	
		}
	}


#		
# option -e error
#

	if(strbuf(j:j) .eq. 'e') {
# check j+1 is blank or null
                        eflag=1
                	if (strbuf(j+1:j+1) .ne. ' ' & 
		         (strbuf(j+1:j+1) .ne. char(0))){
		      		write(ttyout,1210)
1210     	 		format('No blank after -e option')
				go to 2000
			}

			i=i+1
			strbuf = charg(i)
			iopcon=strbuf
			do jtmp= 1,80 {
				if (iopcon(jtmp:jtmp) == char(0)) {
					iopcon(jtmp:jtmp) = ' '
					break 
				}
			}
			k= 1
			call wjfren (k,x,il)

			if (il.ne.0){
				write(ttyout,1220)il,iopcon
1220	format('strange character ',a,'encountered in line: ',a)
 				go to 2000                  
			}
			errmin = x

			i=i+1
1225			strbuf = charg(i)
			iopcon=strbuf
# check for nulls

			do jtmp= 1,80 {
				if (iopcon(jtmp:jtmp) == char(0)) {
					iopcon(jtmp:jtmp) = ' '
					break 
				}
			}
 
			k = 1
			call wjfren(k,x,il)

			if (il .ne. 0) {
				write(ttyout,1020)il,iopcon 
				go to 2000	
			}

			errmax = x

			if (errmin .ge. errmax) { 
				write(ttyout,1230)
1230				format('error mininimum > error maximum')
				go to 2000	
			}
	}





# option -a resolution
#
		if(strbuf(j:j) .eq. 'a') {
                        aflag=1
# check j+1 is blank or null

			if (strbuf(j+1:j+1) .ne. ' ' & 
			    (strbuf(j+1:j+1) .ne. char(0))){
				write(ttyout,1310)
1310     			format('No blank after -a option')
			go to 2000
			}

		i=i+1
		strbuf = charg(i)
		iopcon=strbuf
		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
		k= 1
		call wjfren (k,x,il)

		if (il.ne.0){
			write(ttyout,1320)il,iopcon
1320			format('strange character ', 
				a,'encountered in line: ',a)
 				 go to 2000                  
		}
		asymin = x

		i=i+1
1325		strbuf = charg(i)
		iopcon=strbuf
# check for nulls

		do jtmp= 1,80 {
			if (iopcon(jtmp:jtmp) == char(0)) {
				iopcon(jtmp:jtmp) = ' '
				break 
			}
		}
  
		k = 1
		call wjfren(k,x,il)

		if (il .ne. 0) {
			write(ttyout,1020)il,iopcon 
		go to 2000	
		}

		asymax = x

		if (asymin .ge. asymax) { 
			write(ttyout,1330)
1330			format('asymmetry min > asymmery max ')
		go to 2000	
		}
	}
#
# option -h header to be printed on output list 
#
		if(strbuf(j:j).eq. 'h') head=1
#

#
# option -v verbose : print full title & comment
#
		if(strbuf(j:j).eq. 'v') verb=1
#

#
# option -b band analysis files
#
		if (strbuf(j:j).eq. 'b') {
			band=1
			filnam=bdanalfile
		}
#
	}
	go to 100

# error flag set if invalid option
2000	ferror = 1

3000	return
	end  
