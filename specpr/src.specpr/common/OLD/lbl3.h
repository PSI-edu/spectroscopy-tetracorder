#     /lbl3/ common:
#       ictrl  : control flag for indicating errors are involved or a flag
#         indicating overlaping spectra in the display routine.
#       idad   : flag indicating that erorrs are included in the data (needs
#         to be phased out ).
#       ixit   : flag to indicate that user aborted routine and returned to
#         the main routines.
#
      common /lbl3/ ictrl, idad, ibncon,ibncn2, ixit

      integer*4 ictrl, idad, ibncon,ibncn2, ixit



#######  error  : errors to the data ( 1 standard deviation of the mean ).

      common /lblerr/ error(4864)

      real*4 error
