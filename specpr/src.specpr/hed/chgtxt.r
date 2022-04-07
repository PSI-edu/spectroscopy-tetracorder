	subroutine chgtxt(inxt)
	implicit integer*4 (i-n)

#ccc  name: chgtxt
#ccc  version date: 5/10/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: changes text information in specpr data
#ccc			record by writing text in memory to a data file,
#ccc			invoking an editor, and and then reading the data
#ccc			back to memory.
#ccc
#ccc  algorithm description: none
#ccc  system requirements: must be able to call a system editor
#ccc  subroutines called: system
#ccc  argument list description: inxt
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/lbl6"

	integer*2 chkbit, ibit

	character*80 ifile
	character*1 newlin

	newlin = char(10)     # new line character

# begin:
1	ist = 1
	inxt = 0
	ibit = 1
	
	icrtln = 4
	ipage = 1

	if (chkbit(icflag,ibit) == 0) {
		write (ttyout, 10)
		call crtin
		return
	}

	i = 1

# nextpage:
3	if (ist >= itxtch && ist > 1) {
		inxt = 2
		return
	}

	call eralph
	if (itxtch <= 0) itxtch = 1
	write (ttyout, 5) idv1, ifl1, ititl, ipage, ist, itxtch

	if (itxtch <= 0) go to 4
	while (i <= itxtch) {

		while (i <= itxtch) {
			if (itext(i:i) == newlin) break
			i = i+1
			iend = i
			if (iend - ist >= 80 ) break
		}
		iend = i-1

		if (iend > itxtch) iend = itxtch
		if (iend < ist) iend = ist
		if (iend == ist) {            # case when two newlines in a row
			if (itext(ist:iend) == newlin) {
				write (ttyout, 22)
				iend = iend -1
			} else {              # case for one char, not a newline
				write (ttyout, 20) itext(ist:iend)
			}
		} else {                      # case for multiple characters
			write (ttyout, 20) itext(ist:iend)
		}
		icrtln = icrtln+1
		ist = iend +2
		iend = ist
		i = iend

		if (icrtln > 16) break
	}

4	ipage = ipage +1
	icrtln = 4

	write (ttyout, 30)
	call crtin
	j = 1
	call wjfren (j,x,il)
	if (j >= 80 & x == 0 & il == 0) {
		go to 3		# nextpage
	}
	if (il == ihx || il == ihe || il == ihr || il==ihg) {
		inxt = il
		return
	}

	if (il == ihw) {
		call getstr (j,ifile,ier)
		if (ier == 2) {
			ifile = '.text'
			ier = 0
		}
		if (ier != 0) go to 1		# begin
		open (luntxt, file=ifile, status='unknown',
			access='sequential', form='formatted',
			iostat=ier)
		if (ier != 0) {
			write (ttyout,100) ier, ifile
			call crtin
			go to 1		# begin
		}

		write (luntxt, 20) itext(1:itxtch)

		write (ttyout, '(" closing file and calling editor",/)')
		close (luntxt, iostat=ier)

		call system ('/usr/local/bin/spedit ' // ifile // char(0))

		write (ttyout, '(" reopening file")')

		open (luntxt, file=ifile, status='old',
			access='sequential', form='formatted',
			iostat=ier)
		if (ier != 0) {
			write (ttyout,100) ier, ifile
			call crtin
			go to 1		# begin
		}
		write (ttyout, 200)

		call redtxt (ier)
		close (luntxt, iostat=ier)

		i = 1
		ist = 1
		iend = 1
		go to 1		# begin

	} else if (il == ihi) {
		call getstr (j,ifile,ier)
		if (ier == 2) {
			ifile = '.text'
			ier = 0
		}
		if (ier != 0) go to 1		# begin
		open (luntxt, file=ifile, status='old',
			access='sequential', form='formatted',
			iostat=ier)
		if (ier != 0) {
			write (ttyout,100) ier, ifile
			call crtin
			go to 1		# begin
		}
		write (ttyout, 50) ifile
		call redtxt (ier)
		close (luntxt, iostat=ier)
		go to 1		# begin

	} else {
		go to 3		# nextpage
	}
	
5	format (5x,
		'Header Information Display and Change: Text Change',
		/, 5x, a, i5, 3x, a, /, 'Text: page', i5,
		'; page starts at character', i6, 
		' out of', i6, ' characters',/, 79('-'))

10	format (' Error: text change routine called, but text bit',
		' not set', /,
		' press return to continue',/)

20	format (a)

22	format (' ')

30	format (79('-'), /, 
' Type  w  (filename optional) to write ',
		'data to a file and invoke editor', /,
'       i  (filename optional) to insert ',
		'text from file to memory',/,
'       g  to exit to crt plot,  e  to soft ',
		'exit with write, no plot,',/,
'       x  to hard exit,  r  to return to ',
		'beginning of header info routine',/,
'          press return to display next ',
		'screen of information')

50	format (' reading text from file ', a)
100	format (' OPEN ERROR', i6, ' on file ', a)

200	format (' reading text back into memory',/)

	end
