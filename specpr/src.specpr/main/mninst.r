	subroutine mninst (infopr,id,idvx,idv2,inext,icon,ixit,idisu,
		 idad,ictrl,idx,x,xx)
	implicit integer*4 (i-n)
############################################################

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine gives various options to the
#ccc                   user
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren
#ccc  argument list description:
#ccc       arguments: infopr,id,idvx,idv2,inext,icon,ixit,idisu,idad
#ccc                  ictrl,idx,x,xx
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################

	include "../common/lundefs"


      id=0
      idvx=0
      idv2=0
      inext=0
      icon=2
      ixit=0
      idisu=0
      idad=0
      ictrl=0

      write (ttyout,1)
      if (infopr.eq.1) write (ttyout,2)
      if (infopr.eq.0) write (ttyout,3)
      if (infopr.eq.1) write (ttyout, 46)
      call crtin
      i=1
      call wjfren (i,x,id)
      call wjfren (i,xx,idx)
      return

1     format ( 1x, 'MAIN MENU: ',
		10(1h*), ' Program Operations Control ',21(1h*), /)
2     format (3x, 'INFO:     "in" to turn OFF information',/)
3     format (3x, 'INFO:     "i"  to turn ON  information', /)
46    format (3x,
'LIST:      l followed by v,w,d,u,or y  to list the',
					' contents ',/,25x,
	 			'of the corresponding file', /, 3x,
'DISPLAY:   t  to DISPLAY on screen, OVERLAP on screen',/,3x,
'MATH:      m  to do MATH operations', /, 3x,
'TRANSFER:  t  to TRANSFER (COPY) files',
					//,3x,
'PLOT:      p  to PLOT SPECTRA on PLOTTER/printer', /, 3x,
'SETUP:     b  to change SETUP PARAMETERS', /, 3x,
'FILES:     r  to REASSIGN files and devices', /, 3x,
'STARPACK:  s  to create a STARPACK for extinction corrections',/,3x,
'PRINT RST: f  to print summary of the current restart file',/,3x,
					/, 3x, 
'EXIT:      EX  to exit program',
	  /)

      end
