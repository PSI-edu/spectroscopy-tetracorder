	common /tri1/ inam, inamr, ifils, irecw, inamwi, histfile
	common /tri1/ lunhist, resultfile, lunresult
	common /tri1/ ihbcksl, chdltflg, chtmpc1, chtmpc2
	common /tri1/ lsetup, chfirst, chlast, uname, iversion
	common /tri1/ noflush

	character*8 inam, inamr, inamwi

	character*80    histfile   # history file, file name
	character*80    resultfile # results file, file name
	integer*4       lunhist    # logical unit for writing history info
	integer*4       lunresult  # logical unit for writing results info
        character*1     ihbcksl    # this is the backslash character
	character*1     chdltflg   # delete flag =d to delete points, else =' '
	character*1     chtmpc1    # tmp chas*1 to fill 4 byte word
	character*1     chtmpc2    # tmp chas*1 to fill 4 byte word
	integer*4       lsetup     # reference lib setup flag:
                                   #    = 0 hasn't been done yet
                                   #    = 1 has been done.
	integer*4	irecw      # record number of wavelength set
	integer*4	chfirst    # first spectral channel in sheet to read
	integer*4	chlast     # last spectral channel in sheet to read
                                   # chfirst, chlast is for image cubes.
	character*8	uname      # username
	real*4          iversion   # program version number
	integer*4       noflush    # flush file buffers = 1
                                   # (includes flushseqfile, restart)
                                   # during specorder operations.
	integer*4       ifils      # record number of specpr unknown spectrum
