#  this common block holds the command line arguments submitted with
#  the job
#  ncmdarg is the number of command line arguments
#  charg1 is the actual argument 1

	common /cmdarg/ ncmdarg, charg1, charg2, charg3

	integer*4 ncmdarg
	character*80 charg1, charg2, charg3
