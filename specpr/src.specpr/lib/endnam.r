	subroutine endnam (ifile,ibuf)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine gets the end of the pathway name
#ccc                   of files
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                         none
#ccc  argument list description:
#ccc       arguments: ifile,ibuf
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#***********************************************************************
#   subroutine to get the end of the pathway name of files             *
#***********************************************************************
	character*(*)   ifile,ibuf
	integer*4 crindex

	i = crindex(ifile,'/')

	if (i == 0) ibuf = ifile
	else ibuf = ifile(i+1:len(ifile))

	return
	end
