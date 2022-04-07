	subroutine closef
	implicit integer*4 (i-n)
#########################################################

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine closes all files and devices
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,hreset,whedr,taprw
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
	include "../common/lundefs"

	integer*4 idummy
	call eralph
	call hreset(1)
	write (ttyout,5)
	call whedr
#
	write(ttyout,6)

	close(1,iostat = idummy)
	if (idummy != 0) {
		i = 1
		write (ttyout, 100) ' ', i, idummy
	}

	close(2,iostat = idummy)
	if (idummy != 0) {
		i = 2
		write (ttyout, 100) ' ', i, idummy
	}

	close(3,iostat = idummy)
	if (idummy != 0) {
		i = 3
		write (ttyout, 100) ' ', i, idummy
	}

	close(4,iostat = idummy)
	if (idummy != 0) {
		i = 4
		write (ttyout, 100) ' ', i, idummy
	}

	close(7,iostat = idummy)
	if (idummy != 0) {
		i = 7
		write (ttyout, 100) ' ', i, idummy
	}

	close(8,iostat = idummy)
	if (idummy != 0) {
		i = 8
		write (ttyout, 100) ' ', i, idummy
	}

	close(9,iostat = idummy)
	if (idummy != 0) {
		i = 9
		write (ttyout, 100) ' ', i, idummy
	}

	close(10,iostat = idummy)
	if (idummy != 0) {
		i = 10
		write (ttyout, 100) ' ', i, idummy
	}

	close(12,iostat = idummy)
	if (idummy != 0) {
		i = 12
		write (ttyout, 100) ' ', i, idummy
	}

	close(13,iostat = idummy)
	if (idummy != 0) {
		i = 13
		write (ttyout, 100) ' ', i, idummy
	}

	close(15,iostat = idummy)
	if (idummy != 0) {
		i = 15
		write (ttyout, 100) ' ', i, idummy
	}

	close(16,iostat = idummy)
	if (idummy != 0) {
		i = 16
		write (ttyout, 100) ' ', i, idummy
	}

	close(17,iostat = idummy)
	if (idummy != 0) {
		i = 17
		write (ttyout, 100) ' ', i, idummy
	}

	close(cpylun,iostat=idummy)
	if (idummy != 0) {
		i = cpylun
		write (ttyout, 100) 'cpylun:', i, idummy
	}

	call taprw
	return


5       format (1x,/////////,1x,'restart file ending status:',///)
6       format(/)
100	format (' ERROR on close:',a,' lun:',i6, '    error=',i6)

	end
