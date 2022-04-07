 	subroutine hedr3d (in)   # SCCS ID: %Z% %W% %G%

#*******************************************************************
#   TITLE:               3D HEADER INFORMATION ROUTINE             *
#   PROGRAMMER:          Barry J. Middlebrook                      *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This program is for setting the header information *
#               on 3d files.  Many of the parameters do not apply, *
#               and are set to zero, but are included to allow for *
#               future needs.                                      *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#             hours        - hour of day for data acquisition      *
#             mins         - minutes                               *
#             secs         - seconds                               *
#             er           - error code for input range checking   *
#             month        - month of data acquisition             *
#             day          - day of data acquisition               *
#             year         - year of data acquisition              *
#                                                                  *
#*******************************************************************

#  Set variable type 
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	include        "../common/label1"
	include        "../common/ioftyp"
	include        "../common/lundefs"
	include        "../common/lbl4"
	real*4		secs
 	integer*4      hours,mins,er,month,day,year

#  Get title
	write (ttyout,*)'Please type in the title (16 characters or less).'
	write (ttyout,*)'-----------------|'
	call crtin
	titl3d(ftptr(in))=iopcon(1:16)
	
#  Get time
1	write (ttyout,*)'Please enter the time (hh mm ss) data was acquired'
        write (ttyout,*)'in Universal or Civil time (r to return to main).'
	call crtin
	call getime (hours,mins,secs,er)
	
#  Check for erroneous input
	if (er == 1)  {
	   write (ttyout,*)'ERROR - data entered exceeds valid ranges'
	   write (ttyout,*)'Please re-enter.'; go to 1
	}
	else if (er == 2)  {
	   write (ttyout,*)'Returning to calling program...'
	   return
	}
	else  {
	   continue
	}
	
#  Change time to seconds scaled by 2400 and put into header array
	when(1,ftptr(in))=(hours*3600 + mins*60 + secs)*24000
	
#  Get date
2	write (ttyout,*)'Enter the date (mm dd yyyy) of data acquisition.'
	call crtin
	call getdt (month,day,year,er)

#  Check for invalid input
	if (er == 1)  {
	   write (ttyout,*)'ERROR - data entered exceeds valid ranges'
	   write (ttyout,*)'Please re-enter.' ; go to 2
	}
	else if (er == 2)  {
	   write (ttyout,*)'Returning to calling program...'
	   return
	}
	else  {
	   continue
	}
	
#  Set Julian Day times 10 variable for date
	call tojuld (year,month,day,jdateb)
	when(2,ftptr(in))=jdateb

#  Return to calling program
	return
	end
