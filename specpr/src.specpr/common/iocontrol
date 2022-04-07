#  I/O control flags for redfil and wrifil routines

	common /iocntl/ rferfl(3), idwcon, ioutverbose

	integer*4 rferfl, idwcon, ioutverbose

# rferfl(1): error message control flag 1: continuation bit flag set
#			on read, but fisrt record of data set expected
#		=0 do not issue error message
#		=1 issue error message
#
# rferfl(2): if error associated to above occurs, set = 1
#					     else set = 0
# rferfl(3): unused
#
# idwcon: device file letter ID decoded by devok, upper case.
#		it is assumed that a specpr record will be read,
#		so this ID corresponds to the current file ID,
#		and is used by wavelength routines for auto-wavelength
#		routines.  if this is set (redfil sets it to zero after
#		read), and puts it in itrol(1), and with wavelength
#		pointer, gives auto waves.
#
# ioutverbose   output verbose level to crt for user prompts:
#                this output suppressor is active only when
#                command files are being read.
#                   = 0 do all output.
#                   = 1 supress level 1 output
#                   = 2 supress level 1 and 2 output
#                   = 3 supress level 1, 2, and 3 output
