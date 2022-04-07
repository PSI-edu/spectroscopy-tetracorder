	subroutine rmpath (chbuff,name)

# SCCS ID: %Z% %W% %G%
#*******************************************************************
#   TITLE:             REMOVE PATH ROUTINE                         *
#   PROGRAMMER:        Barry J. Middlebrook                        *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This algorithm is designed to remove the path name *
#               from the full file designation.  It accomplishes   *
#               this by finding the last slash and the first space *
#               in the character buffer holding the file designa-  *
#               tion.  The file name is extracted by setting the   *
#               file name variable equal to the character string   *
#               bounded by last slash position plus 1 and first    *
#               space position minus 1 in the input character buf- *
#               fer.  As an example, consider the file designation *
#                                                                  *
#               u/src/filename                                     *
#               1234567      14                                    *
#                                                                  *
#               The routine finds the last slash index value which *
#               is equal to 6 and the first space index value      *
#               which is equal to 15.  It would then conclude that *
#               the file name is positioned in the input character *
#               buffer from the index 7 to 14.  Therefore,         *
#               <file name> = <character buffer (7:14)>            *
#                                                                  *
#               NOTE:  Path name spaces are allowed, however, the  *
#                      file name will be incorrect if it has       *
#                      any spaces within the name or more than one *
#                      preceding it.                               *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#             maxlen - maximum number of characters in input char- *
#                    acter buffer                                  *
#             i      - integer value for positioning the charac-   *
#                    ter indexer to the next character after the   *
#                    slash found                                   *
#             chtest - integer value that temporarily stores the   *
#                    index of the character searched for           *
#             spapos - first space found position index in input   *
#                    character buffer                              *
#             slapos - last slash found position index in input    *
#             test   - logical variable controlling loop to find   *
#                    last slash                                    *
#             chbuff - input character buffer containing the input *
#                    file designation (path/filename)              *
#             name   - file name without path                      *
#             slash  - character variable containing a slash       *
#             space  - character variable containing a space       *
#                                                                  *
#*******************************************************************

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4       i,chtest,spapos,slapos
	logical         test
	character       chbuff*80,name*8,space,slash
	include		"../common/lundefs"

#  Decode input string comprised of path name and file name 
	#  Initialize variables for test
	space=' '
	slash='/'
	test=.true.
	i=1

#  Test to search through character string for the last slash 
	while (test)  {
	   chtest=index(chbuff(i:maxlen),slash)

	   if (chtest == 0)  {
	      test=.false.
	   }
	   else  {
	      slapos=chtest+slapos
	      i=slapos+1
	   }
	}

#  Test to find position of first space
	spapos=index(chbuff,space)

#  Tests to check for errors in indexing
	if (spapos <= slapos)  {
	   write (ttyout,*)'WARNING - space detected before slash,'
	   write (ttyout,*)'* name may be incorrect *'
	   i=spapos+1
	   chtest=1

	   while (chtest != 0 & spapos < slapos+1)  {
	      chtest=index(chbuff(i:maxlen),space)
	      spapos=spapos+chtest 
	      i=slapos+1
	   }
	}
	else if (spapos == 0)  {
	   write (ttyout,*)'WARNING - no space character detected'
	   spapos=maxlen
	}
	else  {
	   continue
	}

#  Extract file name from full path name
	name=chbuff(slapos+1:spapos-1)   

#  End program and return to calling main
	return
	end
