	subroutine makvicarlabel (ch,lblsiz,form,recsiz,org,nl,ns,nb,
		ctitle,ctask,ier)
	implicit integer*4 (i-n)

#ccc  name:  makvicarlabel
#ccc  version date: 4/24/90
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: makes vicar label and puts it in string ch
#ccc  algorithm description: formatted write to string
#ccc  system requirements: none
#ccc  subroutines called: 
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
#ccc  ch       character array in which to put label
#ccc  lblsiz   label size in bytes (integer*4)
#ccc  form     file format pixel size in bits (integer*4)
#ccc  recsiz   record size in bytes (integer*4)
#ccc  org      file organization: (1=BIL, 2=BIP, 3=BSQ) (integer*4)
#ccc  nl       number of lines in image (integer*4)
#ccc  ns       number of samples in image (integer*4)
#ccc  nb       number of bands in image (integer*4)
#ccc  ctitle   image title (character*(*))
#ccc  ctask    task name (character*(*))
#ccc  ier      return error flag (integer*4)


	character*(*) ch, ctitle, ctask
	character*100 cbuff
	integer*4 lblsiz,form,recsiz,org,nl,ns,nb
	integer*4 ier

	character*1 ichil,iquote,lowch,highch,cnull
	character*3 cm
	character*12 user

	integer*4 ttyout
#RED
	integer*4 lnb     # function
# KEL   declare stlen as int for tetracorder 4.2
	integer*4 stlen

#
###### TEMPROARY:
	ttyout = 6
#
# initialize variables
#
	iquote = char(39)  # a single quote character
	cnull  = char(0)   # null character

#

	stlen = len(ch)


# sample vicar label:

# label size=  1228
# format (pixel size in bits) =    16
# record size=  1228
# file organization=     1
# number of lines=   512
# number of samples=   614
# number of bands=   224
# julian day*10 of data acquisition=    24478185
# time of data acquisition (sec*2400)=   122712000
# julian day*10 of data processing=    24478185
# time of data processing (sec*2400)=   122712000
# title: AVIRIS IMAGE DATA                       
# aviris special title: FLIGHT 18  RUN 005  09/26/89  CRIPPLE CR
#
# LBLSIZE=1228 FORMAT='HALF' TYPE='IMAGE' BUFSIZ=20876 DIM=3 EOL=0
# RECSIZE=1228 ORG='BIL' NL=512 NS=614 NB=224 N1=614 N2=224 N3=512 N4=0
# NBB=0 NLB=0 TASK='AVLOG3' USER='AVIRIS' DAT_TIM='Thu Oct 19 01:25:13
# 1989' A VTITLE='AVIRIS IMAGE DATA ' AVLAB1='FLIGHT 18 RUN 005 09/26/89
# CRIPPLE CREEK SEGMENT 02' AVLAB2='START:  19:14:40 FRAME= 582 LAT=38D
# 26M 08S N LONG=105D 12M 57S W' AVLAB3='STOP:  19:15:50 FRAME= 1426
# LAT=38D 33M 43S N LONG=105D 10M 39S W' TASK='COPY' USER='AVIRIS'
# DAT_TIM='Tue Jan 9 17 :00:13 1990' TASK='AVRAD' USER='AVIRIS'
# DAT_TIM='Tue Jan 9 17:40:23 1990' T ASK='COPY' USER='AVIRIS'
# DAT_TIM='Tue Jan 9 18:21:24 1990'

	ic   = 1
	cbuff=' '
	ch   =' '

# LBLSIZE
	write (cbuff(1:16),1) lblsiz
1	format ('LBLSIZE=',i6,'  ')
	ch(ic:ic+7) = cbuff(1:8)
	ic = ic+8
	numstart = ic
	do ii = 9, 16 {  # skip blanks
		if (cbuff(ii:ii) == ' ') {
			next
		} else {
			ch(ic:ic) = cbuff(ii:ii)
			ic = ic +1
		}
	}
	cbuff=' '
	ch(ic:ic+1) = '  '
	ic = ic+2
	
	ibl = numstart + 16 - ic
	if (ibl > 0) {    # add more blanks
		do ii = 1, ibl {
			ch(ic:ic) = ' '
			ic = ic + 1
		}
	}

# FORMAT
	if (form == 8 ) {
		ch(ic:ic+13) = 'FORMAT=' // iquote // 'BYTE' // iquote // '  '
		ic = ic+15
	} else if (form == 16 ) {
		ch(ic:ic+13) = 'FORMAT=' // iquote // 'HALF' // iquote // '  '
		ic = ic+15
	} else {
		ch(ic:ic+13) = 'FORMAT=' // iquote // '????' // iquote // '  '
		ic = ic+15
	}

# TYPE 
	ch(ic:ic+12) = 'TYPE=' // iquote // 'IMAGE' // iquote // '  '
	ic = ic + 14

# BUFSIZ

	ibufsiz = recsiz*(int(20480./float(recsiz)))
	write (cbuff,2) ibufsiz
2	format ('BUFSIZ=',i5,'   ')
	il = lnb(cbuff)+2
	ch(ic:ic+il) = cbuff(1:il)
	cbuff(1:il)=' '
	ic = ic+il+1

# DIM
	idim = 0
	if (nl > 1) idim = idim +1
	if (ns > 1) idim = idim +1
	if (nb > 1) idim = idim +1
	ch(ic:ic+3) = 'DIM='
	ic = ic+4
	write (ch(ic:ic),'(i1)') idim
	ic = ic +1
	ch(ic:ic+1) = '  '
	ic = ic +2

# EOL
	ch(ic:ic+5) = 'EOL=0  '
	ic = ic +7

# RECSIZE
	write (cbuff,3) recsiz
3	format ('RECSIZE=',i5,'   ')
	ch(ic:ic+7) = cbuff(1:8)
	ic = ic+8
	numstart = ic
	do ii = 9, 16 {  # skip blanks
		if (cbuff(ii:ii) == ' ') {
			next
		} else {
			ch(ic:ic) = cbuff(ii:ii)
			ic = ic +1
		}
	}
	cbuff=' '
	ch(ic:ic+1) = '  '
	ic = ic+2

# ORG (1=BIL, 2=BIP, 3=BSQ) but if dimension =2, then must be BSQ


	if (org == 1 && idim > 2) {
		ch(ic:ic+9) = 'ORG=' // iquote // 'BIL' // iquote // '  '
		ic = ic + 11
	} else if (org == 2 && idim > 2) {
		ch(ic:ic+9) = 'ORG=' // iquote // 'BIP' // iquote // '  '
		ic = ic + 11
	} else if (org == 3 || idim == 2) {
		ch(ic:ic+9) = 'ORG=' // iquote // 'BSQ' // iquote // '  '
		ic = ic + 11
	} else {
		ch(ic:ic+9) = 'ORG=' // iquote // '???' // iquote // '  '
		ic = ic + 11
	}

# NL NS NB N1 N2 N3 N4 NBB NLB

	if (org == 1 && idim > 2) {   #BIL
		write (cbuff(1:87), 4) nl, ns, nb, ns, nb, nl
	} else {
		n1 = 0
		n2 = 0
		n3 = 0
		write (cbuff(1:87), 4) nl, ns, nb, n2, n2, n3
	}
4	format  ( 'NL=',i6,'  NS=',i6,'  NB=',i6,
		'  N1=',i6,'  N2=',i6,'  N3=',i6,
		'  N4=0  NBB=0  NLB=0  ')
	ii = 1
	do ic = ic, ic+87 {                    # compress blanks after =
		ch(ic:ic) = cbuff(ii:ii)
		if (ch(ic:ic) == '=') {
			do j = 1,6 {
				if (cbuff(ii+j:ii+j) != ' ') go to 100
			}
100			ii = ii + j   # break is to here
		} else {
			ii = ii + 1
			if (ii > 87) go to 150
		}
	}


# TASK='____ '

150	itlen = lnb(ctask)
	ch(ic:ic+5) = 'TASK=' // iquote
	ic = ic + 6
	ch(ic:ic+itlen+1) = ctask(1:itlen) // iquote // '  '
	ic = ic+itlen+3

# USER='______'

	ch(ic:ic+5) = 'USER=' // iquote
	ic = ic +6
	user=' '
	ier = fcuser(user)
	if (ier != 0) {
		user='????????'
	}
	do ii = 1,len(user) {
		if (user(ii:ii) == cnull) user(ii:ii) = ' '
	}
	i = lnb(user)
	ch(ic:ic+i+1) = user(1:i) // iquote // '  '
	ic = ic + i +3

# DAT_TIM='Wed Jan 2 11:35:25 1991'

	ch(ic:ic+8) = 'DAT_TIM=' // iquote
	ic = ic +9
	
	call strdate(cbuff)
	i = lnb(cbuff)
	ch(ic:ic+i+1) = cbuff(1:i) // iquote // '  '
	ic = ic + i +3

# TITLE

	itlen = lnb(ctitle)
	ch(ic:ic+6) = 'TITLE=' // iquote
	ic = ic + 7
	ch(ic:ic+itlen+1) = ctitle(1:itlen) // iquote // '  '
	ic = ic+itlen+3

# fill with blanks, terminate with null

	if (ic < stlen) {
		do i = ic, stlen-1 {
			ch(i:i) = ' '
		}
		ch(stlen:stlen) = cnull
	}

	return
	end
