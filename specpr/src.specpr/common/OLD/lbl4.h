#     /lbl4/ common:
#       ierr   : error flag in reading and writing files.
#       bbnd, ubnd : lower and upper bounds for the crt plot vertical axis.
#       iopcon: "option control" ; working array for free format input.
#       ipc   : working array ( general )- used in sequential processor (f2,
#          and f3 ).
#       iops  : options array.
#       istarp: starpack; istarp(1)=s if starpack and istarp(2)=file number.
#         if starpack not requested, both are set to 0.  used in math oper-
#         ations.  division by starpack only.
#       iauto : auto scale flag set to a in display routine if auto scaling is
#         requested by user, and is set to c if not requested.
#       iwdgt : name of data tape for data in file w.
#       isavt :  "   "    "    "   "    "   "  "   v.
#       iwrkt :  "   "    "    "   "    "   "  "   d.
#       inmu  :  "   "    "    "   "    "   "  "   u.
#       inmy  :  "   "    "    "   "    "   "  "   y.
#       wmina and wmaxa are the minimum and maximum wavelengths of the crt
#         plot.  if both are 0, the plot routines auto scale the wavlength
#         ( horizontal ) axis using the wavelength file.
#
        common /lbl4/   wmina,wmaxa, bbnd, ubnd
        common /lbl4/   ipc(10), ierr, istarp(2), iauto
        common /lbl4/   iopcon, iops
        common /lbl4/   iwdgt, isavt,iwrkt, inmu,inmy

        real*4		wmina,wmaxa,bbnd,ubnd
	character*80    iopcon
        character*40    iops
        character*8     iwdgt,isavt,iwrkt,inmu,inmy
        integer*4       ipc, ierr, istarp, iauto
