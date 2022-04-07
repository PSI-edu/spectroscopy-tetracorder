	subroutine refile(inchar,ier)
	implicit integer*4 (i-n)
#ccc  name: refile
#ccc  version date: 07/18/83
#ccc  author(s): J.A.Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: change input device 
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: getstr,wjfren
#ccc  argument list description: inchar,ier
#ccc    inchar  input/output    starting location in iopcon
#ccc    ier     output          error flag. =0 no error
#ccc                            =1 error occured
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
###########################################################
###     this routine changes the input file          ######
###########################################################
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/pipes"

	integer*4 idummy

	logical*4 good

	call getstr(inchar,infile,ier)
	inquire(file=infile,exist=good)
	if (!good) {
		print *,infile,'does not exist'
		ier = 1
		return
	}
	call wjfren(inchar,x,il)
	istart = x
	if (istart!=0) {
		call wjfren(inchar,x,il)
		iend = x
		if (iend==0) iend=2140000000
	} else iend = 2140000000

	if (iend < istart) {
		print *,' starting line > ending line. reenter'
		ier = 1
		return
	}
	pipelv = pipelv +1
	inpipe = pipe(pipelv)
	open (inpipe,file=infile)
	cndx = 0
	redire = .true.
	inline = 1
	while (inline<istart) {
		read(inpipe,'(a)',end=5) iopcon
		inline = inline+1
	}
	ier = 0
	return
5	print *,' EOF before start line'
	ier = 1
	return
	end
