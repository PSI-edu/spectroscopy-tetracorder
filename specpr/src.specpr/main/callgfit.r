	subroutine gfit
	implicit integer*4 (i-n)
#########################################################
#ccc  version date: 06/01/83
#ccc  author(s): Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine calls the gfit program
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc       arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#####################################################
#
#     close all files and devices:
#
	include "../common/lblvol"

	character*40	cmd

	integer*4 ier

	call rstart(1)
	for (i=1; i<5; i=i+1)
		close(i,iostat=ier)
	for (i=7; i<20; i=i+1)
		close(i,iostat=ier)

	cmd = "/usr/bin/gfit " // irfl
	i = system(cmd)
	call rstart(2)
	return
	end
