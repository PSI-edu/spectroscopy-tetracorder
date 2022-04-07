      subroutine inftrd (inftrn)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine does file display, transfer, and
#ccc                   overlay
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: inftrn
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
      include "../common/lundefs"

      write(ttyout,1)
1     format (1x, 10(1h*), ' data DISPLAY, TRANSFER, and OVERLAY',
	  1x, 10(1h*), /)
      if (inftrn.eq.1) {
		write(ttyout, 2)
2     format (' Type  in  to turn OFF information', //,
	' DISPLAY:  Type file id record number and options ',
					'(e.g. v23e w47W65el7)',/,
        ' OVERLAY:  Type file id record number "o" for each overlay',
			'(e.g. v23o w25eo)',/,
	' TRANSFER: file id record number t file id record number.',
                   ' (e.g.v23tv24)', /,
	' MANY TRANSFERS: file id record number +nt file id ',
							 'record number', /,
	'                 where  n  is the number of records-1',
                          ' (e.g. v23+9tw1)')
	write (ttyout,22)
22     format (' ',
	 'DISPLAY AND TRANSFER: file id rec. no. +nct file id rec. no.', /,
	' LIST:    l  followed by v,w,d,u, or y to ',
					'list the contents',/,
				25x,' of the corresponding file',  /,
	' OPTIONS: e=errors, o=overlap, A=auto scale, b=auto scale + ',
						'lp plot', /,
	'          i=display header information only, ',
			'ln=line no., n=0 to 9', /,
	' MATH:    m  go to MATH operations',/,
	' EXIT:    e or x  to EXIT', /)
      } else write(ttyout, 3)
3     format (' Type  i  to turn ON information', /)
      return
      end
