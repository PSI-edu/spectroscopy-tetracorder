	implicit integer*4 (i-n)

	include "../common/cmd"
	include "../common/lbl4"
	include "../common/lundefs"

	icoman = 0
	copy = .false.
	redire = .false.
	iprom = ichar('>')


	open (cmdlun,recl=80,file='.cmd')

	for(;;) {
		call crtin
		i = 1
		while (i<=80) {
			call wjfren(i,x,il)
			write(ttyout,10) i,x,il,il
		}
	}
10      format(' i=',i7,' x=',1pe13.6,' il=',a,i7)
	end


	subroutine er
	print *,'er called'
	return
	end
