subroutine opfile(title )
      
#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
# This subroutine opens the file to which the bandwidths will be
# written and gets the title which will be written  
# the standard common block for header plus complete data set:
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

      include "../common/label1"
      include "../common/lundefs"
      include "../common/alphabet" 



      character*1 method
      character*1 bar,devlet
      character*8 devnam
      character*20 title
      character*80 atemp
      character*15 filnam
      integer*4 blanks
      integer*4 colpos, ios
      integer*4 fileno
      logical*4 ext
      real*4 haxis,error,fwhm,bnddep

#RED Initialize to 0
      ios=0
     
      bar='|' 


# get file name 


10   	call serase(0,310*2,511*2,358*2)
	call movabs(0,349*2)
      call sb(0)
      write(ttyout,201)
201   format('enter filename')
      call movabs(0,339*2)
      call sb(0)
      read(ttyin,fmt=500,err=2000,end=2000) filnam
500   format(a)
      blanks=0
      do colpos=1,15 {
	 if (filnam(colpos:colpos)==' ' ) blanks=blanks+1
      }
      if (blanks==15) goto 10
      inquire(file=filnam,exist=ext,err=2000)
# if ext is true then the file already exits 
      if(ext) {
       		  open(unit=0,file=filnam,status='old',access='sequential',
             				 form= 'formatted',err=3000)

# move to end of previously opened file
#
 
  	      	  while (ios!=-1) {
           		 read(unit=0,fmt=550,iostat=ios) title,bar,haxis,
				error, fwhm,bnddep,coment,devnam,
				fileno,method
         
                  }
      } else {

            open(unit=0,file=filnam,status='new',access='sequential',
     		       form='formatted',err=3000)

      }	



# get title for written data 
	call serase(0,310,511,358)
      call movabs(0,349*2)
      call sb(0)
      write(ttyout,600)
600   format('enter m to input new 20 character title or return',
     	     ' for default')

# show user default title 
#	call serase(0,310,511,358)
      call movabs(0,339*2)
      call sb(0)
      write(ttyout,650)ititl(1:20)
650   format(20a)
      call movabs(0,329*2) 
      call sb(0)
      read(ttyin,fmt=700,err=2000,end=2000)title
700   format(a)
      blanks=0
      do colpos=1, 20  {
         if(title(colpos:colpos)== char(0)) {
            title(colpos:colpos)=' '
	 }
         if(title(colpos:colpos)==' ') {
            blanks=blanks+1
 	 }
# check to see if a return was entered if so use default 
       } 
       if(blanks==20) {
         do colpos=1, 20 {
            title(colpos:colpos)=ititl(colpos:colpos)
         }
       }

# format for the read of the title (20 characters), the | (1
# character), 1 space, the horizontal axis (f7.4), 4 spaces, the error (f6.4),
# 4 spaces, the fwhm (4 spaces), the band depth (f6.4), 4 spaces,
# the comment field  (15 spaces), 4 spaces, the name of the specpr
# file where the data was absconed, the letter of that device and
# the file number   
# I don't really know why I did the read like this instead of a
# character string 

550   format(a,a,1x,f7.4,4x,f6.4,4x,f6.4,4x,f6.4,4x,a,4x,a,i6,a)
      return
2000  call movabs(0,339*2)
      write(ttyout,2010)
2010  format('error inquiring about file exiting')
      return
3000  call movabs(0,339*2)
      write(ttyout,2010)
3010  format('error opening file, exiting')
      return
      end
