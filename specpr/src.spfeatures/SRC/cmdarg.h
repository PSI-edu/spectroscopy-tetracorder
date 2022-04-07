#  this common block holds the command line arguments submitted with
#  the job
#  ncmdarg is the number of command line arguments
#  ic1 is the actual argument 1

	common /cmdarg/ ncmdarg
	common /cmdarg/ charg(20)

	integer*4 ncmdarg
	character*80 charg
