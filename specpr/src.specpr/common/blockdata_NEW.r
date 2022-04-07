	subroutine blockdata
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "alphabet"
	include "filenames"
	include "lblvol"
	include "lbl4"
	include "cmd"
	include "delete"
	include "ioftyp"
	include "pipes"

#	data	iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj,
#		ihk /'a','b','c','d','e','f','g','h','i','j','k'/
#	
#	data	ihl,ihm,ihn,iho,ihp,ihq,ihr,ihs,iht,ihu,
#		ihv /'l','m','n','o','p','q','r','s','t','u','v'/
#
#	data	ihw,ihx,ihy,ihz /'w','x','y','z'/

      iha = ihchar('a')
      ihb = ihchar('b')
      ihc = ihchar('c')
      ihd = ihchar('d')
      ihe = ihchar('e')
      ihf = ihchar('f')
      ihg = ihchar('g')
      ihh = ihchar('h')
      ihi = ihchar('i')
      ihj = ihchar('j')
      ihk = ihchar('k')
      ihl = ihchar('l')
      ihm = ihchar('m')
      ihn = ihchar('n')
      iho = ihchar('o')
      ihp = ihchar('p')
      ihq = ihchar('q')
      ihr = ihchar('r')
      ihs = ihchar('s')
      iht = ihchar('t')
      ihu = ihchar('u')
      ihv = ihchar('v')
      ihw = ihchar('w')
      ihx = ihchar('x')
      ihy = ihchar('y')
      ihz = ihchar('z')


#	data	ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,
#		ihci,ihcj,ihck /'A', 'B', 'C', 'D', 'E',
#		'F', 'G', 'H', 'I', 'J', 'K'/
#
#	data	ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct,
#		ihcu,ihcv /'L', 'M', 'N', 'O', 'P', 'Q', 'R',
#		'S', 'T', 'U', 'V'/
#
#	data	ihcw,ihcx,ihcy,ihcz /'W', 'X', 'Y', 'Z'/

      ihca = ihchar('A')
      ihcb = ihchar('B')
      ihcc = ihchar('C')
      ihcd = ihchar('D')
      ihce = ihchar('E')
      ihcf = ihchar('F')
      ihcg = ihchar('G')
      ihch = ihchar('H')
      ihci = ihchar('I')
      ihcj = ihchar('J')
      ihck = ihchar('K')
      ihcl = ihchar('L')
      ihcm = ihchar('M')
      ihcn = ihchar('N')
      ihco = ihchar('O')
      ihcp = ihchar('P')
      ihcq = ihchar('Q')
      ihcr = ihchar('R')
      ihcs = ihchar('S')
      ihct = ihchar('T')
      ihcu = ihchar('U')
      ihcv = ihchar('V')
      ihcw = ihchar('W')
      ihcx = ihchar('X')
      ihcy = ihchar('Y')
      ihcz = ihchar('Z')


#	data	ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,
#		ih9 /'0','1','2','3','4','5','6','7','8','9'/

      ih0 = ihchar('0')
      ih1 = ihchar('1')
      ih2 = ihchar('2')
      ih3 = ihchar('3')
      ih4 = ihchar('4')
      ih5 = ihchar('5')
      ih6 = ihchar('6')
      ih7 = ihchar('7')
      ih8 = ihchar('8')
      ih9 = ihchar('9')


#	data	ihprd,ihandp /'.','&'/

      ihprd = ihchar('.')
      ihandp = ihchar('&')


	data  ivfl	/NULL/
	data  iwfl	/NULL/
	data  iufl	/NULL/
	data  idfl	/NULL/
	data  iyfl	/NULL/
	data  isfl	/NULL/
	data  ilfl	/SPOOLFL/

	data  iwdgt	/'*unasnd*'/
	data  isavt	/'*unasnd*'/
	data  iwrkt	/'*unasnd*'/
	data  inmu	/'*unasnd*'/
	data  inmy	/'*unasnd*'/


#	data	iprom	/'>'/

      iprom = ihchar('>')

	
	data  delete /-1.23e34/

	data  ftptr /0,0,1,2,0,0,3,4,5,0,0,0,0,0,0,0,0,0,0/

	data  pipe /5,22,23,24,25,26,27,28,29/
	data  pipelv /1/
	
	end
