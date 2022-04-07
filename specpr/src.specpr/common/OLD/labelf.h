# file protection codes
#     /lblelf/ common:
#       file and device pointers and status flags:
#         value      meaning
#          <-3     illegal device or file
#          =-2     device or file not assigned
#          =-1     logical unit number assigned to dummy
#          =0      assigned but not opened
#          >0      assigned and opened: if the file is assigned, the value
#                  points to the next record ( the last record written or
#                  read plus 1 ).
#
#      the variables are for the following devices and files:
#        mag0   : mag tape drive 0
#        mag1   : mag tape drive 1
#        isavf  : file v
#        iwjf   : file w
#        iwrkf  : file d
#        istrf  : file s ( starpack )
#        ilpt   : line printer
#        icdr   : card reader
#        ipp    : printer / plotter
#        isvcu  : file u
#        iwjcy  : file y
#
#
#        if v file is assigned to mag tape 0, then mag0=isavf.  similarily
#        for the other files.


      common /labelf/mag0,mag1,isavf,iwjf,iwrkf,istrf,ilpt,icdr,ipp
      common /labelf/ isvcu,iwjcy
      integer*4 mag0,mag1,isavf,iwjf,iwrkf,istrf,ilpt,icdr,ipp,isvcu,iwjcy
