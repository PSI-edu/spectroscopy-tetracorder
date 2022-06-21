      subroutine wrtenvihdr(lunenvi, envifile, envititle, bndsenvi)

#CCC----------------------------------------------------------------
#C
#C   version date: 09/15/09
#C   author: Eric Livo
#C   language:  Ratfor
#C
#C   short description:  this subroutine writes ENVI header files
#C    for given datacubes (name, dimensions, BIL, Int*16, datacubes)
#C
#C------------------------------------------------------------------

      implicit integer*4 (i-n)

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

      include "multmap.h"
      include "tricube.h"

      integer*4 lunenvi, bndsenvi, lblsiz, lastchar, i

      character*80 envifile
      character*47 envititle
      character*49 envititle2

#  preset variable values
      character*1 envifc, envidt, envibo
      character*13 envift, envistd
      character*3 enviinterleave
      character*11 enviwl

#  append two spaces to title; set control chars to 'space'
      envititle2 = '  ' // envititle
	do i = 1, len(envititle2) {
		if (envititle2(i:i) < char(32)) {
			envititle2(i:i) = ' '  # set to blank
		}
	}

      envifc = '1'
      envift = 'ENVI Standard'
      envidt = '1'
      enviinterleave = 'bsq'
      envistd = 'spectral data'
      envibo = '0'
      enviwl = 'Micrometers'


#  set ENVI bits/pixel to 8 bits
      if(envidt == '1') lblsiz = dx

      lastchar = lnb(envifile)
      open (unit=lunenvi, file=envifile(1:lastchar) // '.gz.hdr', access='sequential',
         form='formatted', status='new', iostat=ier)

      write(lunenvi,'(A)') 'ENVI'
      write(lunenvi,'(A)') 'description = {'
      write(lunenvi,'(A)') envititle2
      write(lunenvi,'(A)') '  }'
      write(lunenvi,*) 'samples = ', dx
      write(lunenvi,*) 'lines   = ', dy
      write(lunenvi,*) 'bands   = ', bndsenvi
      write(lunenvi,*) 'header offset = ', lblsiz
      write(lunenvi,'(A)') 'file compression = ' // envifc
      write(lunenvi,'(A)') 'file type = ' // envift
      write(lunenvi,'(A)') 'data type = ' // envidt
      write(lunenvi,'(A)') 'interleave = ' // enviinterleave
      write(lunenvi,'(A)') 'sensor type = ' // envistd
      write(lunenvi,'(A)') 'byte order = ' // envibo
      write(lunenvi,'(A)') 'wavelength units = ' // enviwl

      close (unit=lunenvi, iostat=ier)

      return
      end
