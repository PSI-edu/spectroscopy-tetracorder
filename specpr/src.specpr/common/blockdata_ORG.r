	block data
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include	"alphabet"
	include "filenames"
	include	"lblvol"
	include	"lbl4"
	include "cmd"
	include "delete"
	include "ioftyp"
	include "pipes"


	data	iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj,
		ihk /'a','b','c','d','e','f','g','h','i','j','k'/
	
	data	ihl,ihm,ihn,iho,ihp,ihq,ihr,ihs,iht,ihu,
		ihv /'l','m','n','o','p','q','r','s','t','u','v'/

	data	ihw,ihx,ihy,ihz /'w','x','y','z'/

	data	ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,
		ihci,ihcj,ihck /'A', 'B', 'C', 'D', 'E',
		'F', 'G', 'H', 'I', 'J', 'K'/
	
	data	ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct,
		ihcu,ihcv /'L', 'M', 'N', 'O', 'P', 'Q', 'R',
		'S', 'T', 'U', 'V'/
 
	data	ihcw,ihcx,ihcy,ihcz /'W', 'X', 'Y', 'Z'/

	data	ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,
		ih9 /'0','1','2','3','4','5','6','7','8','9'/

	data	ihprd,ihandp /'.','&'/

	data	ivfl	/NULL/
	data	iwfl	/NULL/
	data	iufl	/NULL/
	data	idfl	/NULL/
	data	iyfl	/NULL/
	data	isfl	/NULL/
	data	ilfl	/SPOOLFL/

	data	iwdgt	/'*unasnd*'/
	data	isavt	/'*unasnd*'/
	data	iwrkt	/'*unasnd*'/
	data	inmu	/'*unasnd*'/
	data	inmy	/'*unasnd*'/

	data	iprom	/'>'/
	
	data	delete	/-1.23e34/

	data	ftptr /0,0,1,2,0,0,3,4,5,0,0,0,0,0,0,0,0,0,0/

	data	pipe/5,22,23,24,25,26,27,28,29/
	data	pipelv/1/
	
	end
