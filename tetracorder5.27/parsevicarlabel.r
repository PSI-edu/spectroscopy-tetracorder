	subroutine parsevicarlabel (ch,lblsiz,form,recsiz,org,nl,ns,nb,
		ivdate,ivtime,ivdat2,ivtim2,ivtitl,avlab1,ier)
	implicit integer*4 (i-n)

#ccc  name:  parsevicarlabel
#ccc  version date: 4/24/90
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: parse vicar label to get parameters needed
#ccc
#ccc  algorithm description: search strings in the vicar label for
#ccc                         various fields
#ccc  system requirements: none
#ccc  subroutines called:       wjfren
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

# Note: Windows PCs need ".h" at end of include file (or use quotes)
	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

        include '../specpr/src.specpr/common/label1'
        include '../specpr/src.specpr/common/lbl4'
        include '../specpr/src.specpr/common/lundefs'

	character*1536 ch
	character*40 ivtitl,avlab1
	integer*4 lblsiz,form,recsiz,org,nl,ns,nb
	integer*4 ivdate,ivtime,ivdat2,ivtim2,ier

	character*1 ichil,iquote,lowch,highch,cnull
	character*40 itlsav
	character*80 filnam
	character*3 cm

#	integer*4 ttyout
#
###### TEMPROARY:
#	ttyout = 6
#
# initialize variables
#
	iquote = char(39)  # a single quote character

	do i = 1, 40 {
		ivtitl(i:i)=' '
		avlab1(i:i)=' '
	}
#
# check that all characters are in range, if not, set to blank.
#
	lowch = char(32)     # lowest character (lower ones are control ch)
	highch = char(126)   # highest character (we exclude 8-bit, and DEL)
	cnull = char(0)      # null character
	ii = len(ch)
	do i = 1, ii {       # this starts the "check do loop"
		if (ch(i:i) == cnull && i < ii) {    # blank out rest of
							# label after find
							# first null.
			do jj = i, ii {
				ch(jj:jj) = ' '
			}
			# this breaks out of the "check do loop"
			   # (NOTE: specpr version ratfor fix:
			   # don't put comment after the break [break-comment line is bad])
			   # possibly associated with end do loop as well
			break
		}
		if (ch(i:i) < lowch || ch(i:i) > highch) {
			ch(i:i) = ' '                     # set to blank
		}
	}       		# this ends the "check do loop"
#
	lblsiz=0
	form=0
	recsiz=0
	org=0
	nl=0
	ns=0
	nb=0
	ivdate=0
	ivtime=0
	ivdat2=0
	ivtim2=0
	ier = 0

	isgn = 1

	do ii = 1, 100 {    # search for LBLSIZE
		if (ch(ii:ii+7) == 'LBLSIZE=') {
			i = ii+8
			iopcon=ch(i:i+79)
			j = 1
			call wjfren(j,x,il)
			if (il == 0 && x > 0) {
				lblsiz = x
				break
			} else {
				call what(j)
				write (ttyout,10)
10				format ('ERROR decoding label size')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 200 {                       # search for FORMAT
		if (ch(ii:ii+6) == 'FORMAT=') {
			i=ii+8
			if (ch(i:i+3) == 'HALF') {
				form = 16
				break
			} else if (ch(i:i+3) == 'BYTE') {
				form = 8
				break
			} else if (ch(i:i+3) == 'REAL') {
				form = 1
				break
			} else if (ch(i:i+3) == 'FULL') {
				form = 32
				break
			} else {
				write (ttyout,20)
20				format('CANNOT decode format')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 900 {    # search for RECSIZE
		if (ch(ii:ii+7) == 'RECSIZE=') {
			i = ii+8
			iopcon=ch(i:i+79)
			j = 1
			call wjfren(j,x,il)
			if (il == 0 && x > 0) {
				recsiz = x
				break
			} else {
				call what(j)
				write (ttyout,30)
30				format ('ERROR decoding record size')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 900 {                       # search for ORG
		if (ch(ii:ii+3) == 'ORG=') {
			i=ii+5
			if (ch(i:i+2) == 'BIL') {
				org = 1
				break
			} else if (ch(i:i+2) == 'BIP') {
				org = 2
				break
			} else if (ch(i:i+2) == 'BSQ') {
				org = 3
				break
			} else {
				write (ttyout,40)
40				format('CANNOT decode file organization')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 900 {    # search for NL=  (number of lines)
		if (ch(ii:ii+2) == 'NL=') {
			i = ii+3
			iopcon=ch(i:i+79)
			j = 1
			call wjfren(j,x,il)
			if (il == 0 && x > 0) {
				nl = x
				break
			} else {
				call what(j)
				write (ttyout,50)
50				format ('ERROR decoding number of lines')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 900 {    # search for NS= (number of samples)
		if (ch(ii:ii+2) == 'NS=') {
			i = ii+3
			iopcon=ch(i:i+79)
			j = 1
			call wjfren(j,x,il)
			if (il == 0 && x > 0) {
				ns = x
				break
			} else {
				call what(j)
				write (ttyout,60)
60				format ('ERROR decoding number of samples')
				ier = 1
				return
			}
		}
	}

	do ii = 1, 900 {    # search for NB=
		if (ch(ii:ii+2) == 'NB=') {
			i = ii+3
			iopcon=ch(i:i+79)
			j = 1
			call wjfren(j,x,il)
			if (il == 0 && x > 0) {
				nb = x
				break
			} else {
				call what(j)
				write (ttyout,70)
70				format ('ERROR decoding number of bands')
				ier = 1
				return
			}
		}
	}

# 1989' AVTITLE='AVIRIS IMAGE DATA ' AVLAB1='FLIGHT 18 RUN 005 09/26/89
	do ii = 1, 1000 {    # search for TITLE=
		if (ch(ii:ii+5) == 'TITLE=') {
			i = ii+7
			iopcon=ch(i:i+79)
			do jj = 1, 40 {
				if (iopcon(jj:jj) != iquote) {
					ivtitl(jj:jj)=iopcon(jj:jj)
					jjj = jj +1
				} else {
					jjj = jj
					break
				}
			}
			if (jjj <= 40) {
				do jj= jjj, 40 {
					ivtitl(jjj:jjj)=' '
				}
			}
			break
		}
	}

	do ii = 1, 1000 {    # search for AVLAB1=
		if (ch(ii:ii+6) == 'AVLAB1=') {
			i = ii+8
			iopcon=ch(i:i+79)
			do jj = 1, 40 {
				if (iopcon(jj:jj) != iquote) {
					avlab1(jj:jj)=iopcon(jj:jj)
					jjj = jj +1
				} else {
					jjj = jj
					break
				}
			}
			if (jjj <= 40) {
				do jj= jjj, 40 {
					avlab1(jjj:jjj)=' '
				}
			}
			break
		}
	}

	do ii = 1, 1000 {    # search for DAT_TIM=
                             # change = to X so won't do because JPL changed format
		if (ch(ii:ii+7) == 'DAT_TIMX') {
			i = ii+9
			iopcon=ch(i:i+79)
			do jj = 1, 80 {
				if (iopcon(jj:jj) == iquote) {
					do jjj = jj, 80 {
						iopcon(jjj:jjj)=' '
					}
					break
				}
			}
			i = 5
			iday = 0
			if (iopcon(i:i) == '0' & iopcon(i:i) == '1' &
					iopcon(i:i) == '2' &
					iopcon(i:i) == '3') { # day month mode

				call wjfren(i,x,il)
				if (il == 0 || il == ihchar('-')) iday = x
				i = i +1
			}

			call str2tmonth (iopcon(i:i+2), mon)
			if (mon < 1) {
				ier = 1
				return
			}
			i = i+4
			if (day == 0) {    # if day > 0 already got day
				call wjfren (i,x,il)
				if (il != 0) {
					write (ttyout,90) il
90					format ('ERROR decoding day of month:',
						' strange character ',a1,
						' encountered.')
					ier = 1
					return
				} else if (x < 1 || x > 31) {
					write (ttyout, 92) x
92					format ('ERROR: day of month ',
						'out of range:', f7.0)
					ier = 1
					return
				}
				iday = x + 0.5
			} else {               # need to get year
				call wjfren (i,x,il)
				iy = x
			}
			call wjfren(i,x,il)
			if (il != 0 & il != ihchar(':')) {
				write (ttyout,110)il 
110				format (1x, 'ERROR getting time: ',
					a1, ' encountered',/)
				ier = 1
				return
			}
			id = x + 0.5
	
			call wjfren(i,x,il)
			if (il != 0 & il != ihchar(':')) {
				write (ttyout,110)il 
				ier = 1
				return
			}
			im = x + 0.5

			call wjfren(i,x,il)
			if (il != 0 & il != ihchar(':')) {
				write (ttyout,110)il 
				ier = 1
				return
			}
			sec = x
			isgn = 1
#DEBUG:
#			write (6,*)'id=',id,'im=',im,'sec=',sec,'isgn=',isgn

			iscale = 24000
			call frdms(itime,iscale,id,im,sec,isgn)

			ivtime = itime
			ivtim2 = itime

			call wjfren(i,x,il)
			if (il != 0) {
				write (ttyout,112) il
112				format ('ERROR getting year: ',
					'strange character ',a1,
					'encountered')
				ier = 1
				return
			}
			iy = x + 0.5

			call  tojuld(iy,mon,iday,jday)
			ivdate = jday
			ivdat2 = jday

			break
		}
	}


# a typical (aviris) vicar label:
#
# LBLSIZE=1228 FORMAT='HALF' TYPE='IMAGE' BUFSIZ=20876 DIM=3 EOL=0
# RECSIZE=1228 ORG='BIL' NL=512 NS=614 NB=224 N1=614 N2=224 N3=512 N4=0
# NBB=0 NLB=0 TASK='AVLOG3' USER='AVIRIS' DAT_TIM='Thu Oct 19 01:25:13
# 1989' AVTITLE='AVIRIS IMAGE DATA ' AVLAB1='FLIGHT 18 RUN 005 09/26/89
# CRIPPLE CREEK SEGMENT 02' AVLAB2='START:  19:14:40 FRAME= 582 LAT=38D
# 26M 08S N LONG=105D 12M 57S W' AVLAB3='STOP:  19:15:50 FRAME= 1426
# LAT=38D 33M 43S N LONG=105D 10M 39S W' TASK='COPY' USER='AVIRIS'
# DAT_TIM='Tue Jan 9 17:00:13 1990' TASK='AVRAD' USER='AVIRIS'
# DAT_TIM='Tue Jan 9 17:40:23 1990' TASK='COPY' USER='AVIRIS' DAT_TIM='Tue
# Jan 9 18:21:24 1990' UD2:[AVIRIS.AVCAL]890413.SPC;1

##



# FOLLOWING CODE IS FROM CGASTOSP FOR EXAMPLE DATE AND TIME STUFF

	if (iopcon(1:9).eq.'DATE-OBS=') {
		do j= 1,80 {
			if(iopcon(j:j)=='-'){
				iopcon(j:j)='/'
			}
		}
		i=12
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (ttyout,120) il
120			format (1x, 'error getting jdatea: ', a1,
				' encountered',/)
		}
		iday = x + 0.5
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (ttyout,120) il
		}
		imon = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (ttyout,120) il
		}
		iy = x + 1900.5

		call  tojuld(iy,imon,iday,jday)
		jdatea = jday
		jdateb = jday
#
#               time is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(24:25)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(26:27)
		iopcon(6:6) = ':'
		iopcon(7:8) = iopcon(28:29)
		iopcon(9:9) = ' '

		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,130)il 
130			format (1x, 'error getting observation time: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,130)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,130)il 
		}
		sec = x
		isgn = 1
#DEBUG:
#		write (6,*)'id=',id,'im=',im,'sec=',sec,'isgn=',isgn

		iscale = 24000
		call frdms(itime,iscale,id,im,sec,isgn)

		iscta = itime
		isctb = itime
	}
#
	if (iopcon(1:9).eq.'RA      =') { 
		isgn = 1
#
#               RA is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(22:23)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(24:25)
		iopcon(6:6) = ':'
		iopcon(7:11) = iopcon(26:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,131)il 
131			format (1x, 'error getting RA: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,131)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,131)il 
		}
		sec = x
		iscale = 1000
		call frdms(isra,iscale,id,im,sec,isgn)

	}
#
	if (iopcon(1:9).eq.'DEC     =') { 
#
#               DEC is scrunched together, so extract it and put in :
#
		isgn = 1
		do ii = 21, 28 {
			if (iopcon(ii:ii) == '-') {
				iopcon(ii:ii) = ' '
				isgn = -1
			}
		}
		iopcon(1:3) = iopcon(22:24)
		iopcon(4:4) = ':'
		iopcon(5:6) = iopcon(25:26)
		iopcon(7:7) = ':'
		iopcon(8:11) = iopcon(27:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,132)il 
132			format (1x, 'error getting DEC: ', a1,
				' encountered',/)
		}
		id = abs(x) + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,132)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,132)il 
		}
		sec = x
		iscale = 1000
		call frdms(isdec,iscale,id,im,sec,isgn)

	}
#
	if (iopcon(1:9).eq.'HA      =') { 
		isgn = 1
		do ii = 20, 28 {
			if (iopcon(ii:ii) == '-') {
				iopcon(ii:ii) = ' '
				isgn = -1
			}
		}
#
#               HA is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(22:23)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(24:25)
		iopcon(6:6) = ':'
		iopcon(7:11) = iopcon(26:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,133)il 
133			format (1x, 'error getting HA: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,133)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (ttyout,133)il 
		}
		sec = x
		iscale = 24000
		call frdms(isha,iscale,id,im,sec,isgn)
#
#               now ST = RA + HA
#
		istb = isra*24 + isha
#
		isecperday = 2073600000  # this = seconds per day * 24000
		if (istb > isecperday) {
			istb = istb - isecperday
		}
		if (istb < 0) {
			istb = istb + isecperday
		}

	}
#
#
	return
	end
