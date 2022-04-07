	subroutine rederr(irecd, lun, ier)
	implicit integer*4 (i-n)

#ccc  name: rederr
#ccc  version date: 5/16/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: The routine reads the error record associated
#ccc			 with data record lun, irecd
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: redfil
#ccc  argument list description: lun= logical unit number
#ccc				 irecd = record number of the data record
#ccc					 (error record comes after the data)
#ccc				 ier = return read error flag
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:	Because of the continuation records in the new specpr data
#ccc		data format, the error is not always at location 
#ccc		data record +1.  So, this routine first reads the data record
#ccc		and then the error record is known, so the error record is
#ccc		read.
#ccc

	itmp = irecd

# read data record.  Use temporary record variable, because redfil
#			modifies it if there are continuation records

	call redfil (itmp, lun, ier)
	if (ier != 0) return

# now read error record

	itmp = itmp+1
	call redfil (itmp, lun, ier)

	return
	end
